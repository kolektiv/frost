namespace Frost.Core

open System
open System.Collections.Generic
open System.Threading.Tasks
open FSharpx
open FSharpx.Lens.Operators
open FSharpx.Async


[<AutoOpen>]
module Types = 

    // Frost

    type FrostEnv = IDictionary<string, obj>
    type FrostApp = FrostEnv -> Async<FrostEnv>
    type FrostAppFunc = Func<FrostEnv, Task>

    // HTTP

    type Method =
        | DELETE | HEAD | GET | OPTIONS 
        | PATCH | POST | PUT | TRACE | Custom of string

        static member fromString =
            function | "DELETE" -> DELETE | "HEAD" -> HEAD 
                     | "GET" -> GET | "OPTIONS" -> OPTIONS
                     | "PATCH" -> PATCH | "POST" -> POST 
                     | "PUT" -> PUT | "TRACE" -> TRACE
                     | x -> Custom x 

        static member toString =
            function | DELETE -> "DELETE" | HEAD -> "HEAD" 
                     | GET -> "GET" | OPTIONS -> "OPTIONS"
                     | PATCH -> "PATCH" | POST -> "POST" 
                     | PUT -> "PUT"  | TRACE -> "TRACE"
                     | Custom x -> x

    type Protocol =
        | HTTP of float | Custom of string

        static member fromString =
            function | "HTTP/1.0" -> HTTP 1.0 
                     | "HTTP/1.1" -> HTTP 1.1 
                     | x -> Custom x

        static member toString =
            function | HTTP x -> sprintf "HTTP/%f" x 
                     | Custom x -> x

    type Scheme =
        | HTTP | HTTPS | Custom of string

        static member fromString =
            function | "http" -> HTTP 
                     | "https" -> HTTPS 
                     | x -> Custom x

        static member toString =
            function | HTTP -> "http" 
                     | HTTPS -> "https" 
                     | Custom x -> x


[<AutoOpen>]
module Lenses =

    let private key<'T> key : Lens<FrostEnv, 'T option> =
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

    let private opt = Lens.xmap (Option.get) (Some) Lens.id
    let private xmapMethod = Lens.xmap Method.fromString Method.toString Lens.id
    let private xmapProtocol = Lens.xmap Protocol.fromString Protocol.toString Lens.id
    let private xmapScheme = Lens.xmap Scheme.fromString Scheme.toString Lens.id
    let private xmapList= Lens.xmap (Option.map List.ofArray) (Option.map List.toArray) Lens.id

    let prop<'T> k = key<'T> k


    [<RequireQualifiedAccess>]
    module Common =
    
        open System.IO

        let Trace = key<TextWriter> "host.TraceOutput"


    [<RequireQualifiedAccess>]
    module Owin =

        open System.Threading

        let Cancellation = key<CancellationToken> "owin.CallCancelled" >>| Lens.xmap Option.get Some Lens.id
        let Version = key<string> "owin.Version"


    [<RequireQualifiedAccess>]
    module Request =

        open System.IO

        let private Headers = key<IDictionary<string, string []>> "owin.RequestHeaders" >>| Lens.xmap Option.get Some Lens.id

        let Body = key<Stream> "owin.RequestBody" >>| Lens.xmap Option.get Some Lens.id
        let Header key = Headers >>| header key >>| xmapList
        let Method = key<string> "owin.RequestMethod" >>| opt >>| xmapMethod
        let Path = key<string> "owin.RequestPath" >>| opt
        let PathBase = key<string> "owin.RequestPathBase" >>| opt
        let Protocol = key<string> "owin.RequestProtocol" >>| opt >>| xmapProtocol
        let QueryString = key<string> "owin.RequestQueryString" >>| opt
        let Scheme = key<string> "owin.RequestScheme" >>| opt >>| xmapScheme


    [<RequireQualifiedAccess>]
    module Response =

        open System.IO

        let private Headers = key<IDictionary<string, string []>> "owin.ResponseHeaders" >>| Lens.xmap Option.get Some Lens.id

        let Body = key<Stream> "owin.ResponseBody" >>| Lens.xmap Option.get Some Lens.id
        let Header key = Headers >>| header key >>| xmapList
        let StatusCode = key<int> "owin.ResponseStatusCode"
        let ReasonPhrase = key<string> "owin.ResponseReasonPhrase"
        let Protocol = key<string> "owin.ResponseProtocol" >>| opt >>| xmapProtocol // TODO - response protocol is really optional...


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

    // Monadic Lens Functions

    let get l = 
        frost { 
            return! fun s -> 
                async { return Lens.get s l, s } }

    let set l v =
        frost { 
            do! fun s -> 
                async { return (), Lens.set v s l } }

    let update l f = 
        frost { 
            do! fun s -> 
                async { return (), Lens.update f l s } }

    // Compilation

    let compile (f: Frost<_>) : FrostApp =
        fun e -> async { return! snd <!> f e }

    // Utility

    let frostAppToFunc (app: FrostApp) : FrostAppFunc =
        Func<_,_> (fun e -> Async.StartAsTask (app e) :> Task)


module Frost =

    let async f =
        frost {
            return! fun s -> 
                async { return! flip tuple2 s <!> f } }

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


module Operators =

    // Monadic Operators

    let inline (!!!) f = Frost.async f
    let inline (>=>) m f = Frost.compose m f
    let inline (<=<) f m = Frost.compose m f
    let inline (>>=) m f = Frost.composeSeq m f
    let inline (=<<) f m = Frost.composeSeq m f
    let inline (<!>) f m = Frost.map f m

    // Lens Operators

    let inline (!!) l = get l
    let inline (=>) v l = set l v
    let inline (=!>) f l = update l f
