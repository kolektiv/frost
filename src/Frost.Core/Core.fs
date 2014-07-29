namespace Frost.Core

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharpx
open FSharpx.Lens.Operators
open FSharpx.Async


[<AutoOpen>]
module Types = 

    type FrostEnv = IDictionary<string, obj>
    type FrostApp = FrostEnv -> Async<FrostEnv>
    type FrostAppFunc = Func<FrostEnv, Task>


[<AutoOpen>]
module Lenses =

    let key<'T> key : Lens<FrostEnv, 'T option> =
        { Get = fun env ->
              match env.TryGetValue key with
              | true, value -> Some (value :?> 'T)
              | _ -> None
          Set = fun value env ->
              match value with
              | Some value -> env.[key] <- box value; env
              | _ -> env.Remove key |> ignore; env }

    let private header key : Lens<IDictionary<string, string []>, string [] option> =
        { Get = fun headers ->
              match headers.TryGetValue key with
              | true, header -> Some header
              | _ -> None
          Set = fun header headers ->
              match header with
              | Some header -> headers.[key] <- header; headers
              | _ -> headers.Remove key |> ignore; headers }

    let xmapOpt<'T, 'U> (f: 'T -> 'U) (t: 'U -> 'T) =
        Lens.xmap (Option.map f) (Option.map t) Lens.id

    let xmapReq<'T, 'U> (f: 'T -> 'U) (t: 'U -> 'T) =
        Lens.xmap (Option.get >> f) (t >> Some) Lens.id

    let required<'T> = 
        xmapReq<'T, 'T> id id


    [<RequireQualifiedAccess>]
    module Common =
    
        open System.IO

        let Trace = key<TextWriter> "host.TraceOutput"


    [<RequireQualifiedAccess>]
    module Owin =

        open System.Threading

        let Cancellation = key<CancellationToken> "owin.CallCancelled" >>| required
        let Version = key<string> "owin.Version"


    [<RequireQualifiedAccess>]
    module Request =

        open System.IO

        let private Headers = key<IDictionary<string, string []>> "owin.RequestHeaders" >>| required
        let private QueryString = key<string> "owin.RequestQueryString" >>| (QueryString.map ||> xmapReq)

        let Body = key<Stream> "owin.RequestBody" >>| required
        let Header key = Headers >>| header key >>| xmapOpt List.ofArray List.toArray
        let Method = key<string> "owin.RequestMethod" >>| (Method.map ||> xmapReq)
        let Path = key<string> "owin.RequestPath" >>| required
        let PathBase = key<string> "owin.RequestPathBase" >>| required
        let Protocol = key<string> "owin.RequestProtocol" >>| (Protocol.map ||> xmapReq)
        let Query k = QueryString >>| Lens.forMap k
        let Scheme = key<string> "owin.RequestScheme" >>| (Scheme.map ||> xmapReq)


    [<RequireQualifiedAccess>]
    module Response =

        open System.IO

        let private Headers = key<IDictionary<string, string []>> "owin.ResponseHeaders" >>| required

        let Body = key<Stream> "owin.ResponseBody" >>| required
        let Header key = Headers >>| header key >>| xmapOpt List.ofArray List.toArray
        let StatusCode = key<int> "owin.ResponseStatusCode"
        let ReasonPhrase = key<string> "owin.ResponseReasonPhrase"
        let Protocol = key<string> "owin.ResponseProtocol" >>| (Protocol.map ||> xmapOpt)


[<AutoOpen>]
module Monad =

    // Monad Type

    type Frost<'T> = 
        FrostEnv -> Async<'T * FrostEnv>

    // Monad Builder

    type FrostBuilder () =

        member x.Return t : Frost<_> = 
            fun s -> async.Return (t, s)

        member x.ReturnFrom f : Frost<_> = 
            f

        member x.Zero () : Frost<unit> = x.Return ()

        member x.Bind (m: Frost<_>, k: _ -> Frost<_>) : Frost<_> =
            fun state -> 
                async { 
                    return! m state >>= fun (result, state) -> (k result) state }

        member x.Delay (f: unit -> Frost<_>) : Frost<_> = 
            x.Bind (x.Return (), f)

        member x.Combine (r1: Frost<_>, r2: Frost<_>) : Frost<_> = 
            x.Bind (r1, fun () -> r2)

        member x.TryWith (body: Frost<_>, handler: exn -> Frost<_>) : Frost<_> =
            fun state -> 
                try body state 
                with ex -> 
                    handler ex state

        member x.TryFinally (body: Frost<_>, handler) : Frost<_> =
            fun state -> 
                try body state 
                finally handler ()

        member x.Using (resource: #IDisposable, body: _ -> Frost<_>) : Frost<_> =
            x.TryFinally (body resource, fun () ->
                match box resource with
                | null -> ()
                | _ -> resource.Dispose ())

        member x.While (guard, body: Frost<_>) : Frost<unit> =
            match guard () with
            | true -> x.Bind (body, fun () -> x.While (guard, body))
            | _ -> x.Zero ()

        member x.For (sequence: seq<_>, body: _ -> Frost<_>) : Frost<unit> =
            x.Using (sequence.GetEnumerator (),
                (fun enum ->
                    x.While (enum.MoveNext, x.Delay (fun () ->
                        body enum.Current))))

    // Monad Expression

    let frost = FrostBuilder ()


[<AutoOpen>]
module Functions =

    // Lens

    let get l : Frost<_> = 
        fun s -> async { return Lens.get s l, s }

    let set l v : Frost<unit> =
        fun s -> async { return (), Lens.set v s l }

    let update l f : Frost<unit> =
        fun s -> async { return (), Lens.update f l s }

    // Monad


    // Compilation

    let compile f : FrostApp = 
        fun e -> async { return! snd <!> f e }

    // Utility

    let toFunc app : FrostAppFunc =
        Func<_,_> (fun e -> Async.StartAsTask (app e) :> Task)


module Frost =

    // TODO: Look at tidying this up? Possibly some of these should be a bit different...

    let async f : Frost<_> =
        fun s -> async { return! flip tuple2 s <!> f }

    let chain m1 m2 : Frost<_> =
        frost {
            do! m1
            return! m2 }

    let pipe m1 m2 : Frost<bool> =
        frost {
            let! next = m1

            match next with
            | true -> return! m2
            | _ -> return false }

    let compose m f =
        fun x ->
            frost {
                let! v = m x
                return! f v }

    let composeSeq m f =
        frost {
            let! v = m
            return! f v }

    let map f m =
        frost {
            let! v = m
            return f v }

    let seq ms =
        frost {
            for m in ms do 
                do! m }


    let memo n f =
        frost {
            let! memoized = get <| key n

            match memoized with
            | Some memoized ->
                return memoized
            | _ ->
                let! created = f
                do! set (key n) (Some created)

                return created }

    let tuple2 m1 m2 =
        frost {
            let! x = m1
            let! y = m2
            
            return x, y }

    let tuple3 m1 m2 m3 =
        frost {
            let! x = m1
            let! y = m2
            let! z = m3
            
            return x, y , z}


module Operators =

    // Monadic Operators

    let inline (!!!) f = Frost.async f
    let inline (>=>) m f = Frost.compose m f
    let inline (<=<) f m = Frost.compose m f
    let inline (>>=) m f = Frost.composeSeq m f
    let inline (=<<) f m = Frost.composeSeq m f
    let inline (<!>) f m = Frost.map f m
    let inline (>?>) m1 m2 = Frost.pipe m1 m2
    let inline (>->) m1 m2 = Frost.chain m1 m2
    let inline (@>>) n f = Frost.memo f n
    let inline (<<@) f n = Frost.memo f n

    // Lens Operators

    let inline (-->) v l = set l v
    let inline (<--) l v = set l v
    let inline (-!>) f l = update l f
    let inline (<!-) l f = update l f
