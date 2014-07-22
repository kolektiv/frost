namespace Frost.Resource

open FSharpx
open FSharpx.Lens.Operators
open Frost.Core
open Frost.Core.Operators


[<AutoOpen>]
module Types =

    // Resource Definition

    type FrostResourceDef =
        { Actions: Map<string, Action>
          Configuration: Map<string, obj>
          Decisions: Map<string, Decision> 
          Handlers: Map<string, Handler> }

        static member empty =
            { Actions = Map.empty
              Configuration = Map.empty
              Decisions = Map.empty
              Handlers = Map.empty }

        static member actions =
            { Get = fun x -> x.Actions
              Set = fun a x -> { x with Actions = a } }
        
        static member decisions =
            { Get = fun x -> x.Decisions
              Set = fun d x -> { x with Decisions = d } }

        static member config =
            { Get = fun x -> x.Configuration
              Set = fun c x -> { x with Configuration = c } }

        static member handlers =
            { Get = fun x -> x.Handlers
              Set = fun h x -> { x with Handlers = h } }

    and Action = Frost<unit>
    and Decision = Frost<bool>
    and Handler = Frost<obj>

    // Resource Execution Graph

    type internal FrostResourceGraph =
        Map<string, FrostResourceNode>

    and FrostResourceNode =
        | Action of Action * string       
        | Decision of Decision * (string * string)
        | Handler of Handler


[<AutoOpen>]
module Monad =

    // Monad Type

    type FrostResource = 
        FrostResourceDef -> unit * FrostResourceDef

    // Monad Builder

    type FrostResourceBuilder () =

        member x.Return (t) : FrostResource = 
            tuple2 t

        member x.ReturnFrom f : FrostResource = 
            f

        member x.Bind (m, k) : FrostResource = 
            m >> fun (result, resource) -> (k result) resource

        member x.Combine (r1, r2) : FrostResource = 
            x.Bind (r1, fun () -> r2)

        member internal x.Set (r, lens, value) = 
            x.Bind ((fun res -> (), Lens.set value res lens), fun _ -> x.ReturnFrom r)
    
    // Monad Expression

    let frostResource = FrostResourceBuilder ()


[<AutoOpen>]
module Lenses =

    let internal action k = FrostResourceDef.actions >>| Lens.forMap k
    let internal config k = FrostResourceDef.config >>| Lens.forMap k
    let internal decision k = FrostResourceDef.decisions >>| Lens.forMap k
    let internal handler k = FrostResourceDef.handlers >>| Lens.forMap k    


    [<RequireQualifiedAccess>]
    module Resource =

        let Definition = prop<FrostResourceDef> "frost.resourceDefinition"


[<AutoOpen>]
module Functions =

    module A = Actions
    module C = Config
    module D = Decisions
    module H = Handlers

    // Helpers

    let private getOrElse l k a r =
        Lens.get r (l k) |> Option.getOrElse a

    // Handlers

    let internal defaultH code phrase =
        frost {
            do! Some code => Response.StatusCode
            do! Some phrase => Response.ReasonPhrase

            return (box "") }

    // Predicates

    let internal trueP = 
        frost { return true }

    let internal falseP = 
        frost { return false }

    let internal unitP = 
        frost { return () }

    let internal headerExists h =
        frost { return! Option.isSome <!> !! (Request.Header h) }

    let internal isMethod m =
        frost { return! (=) m <!> !! Request.Method }

    // Definition

    let private defFrom (r: FrostResource) =
        r FrostResourceDef.empty |> snd

    // Nodes

    let private actions =
        [ A.Delete, unitP, D.Deleted
          A.Patch, unitP, D.RespondWithEntity
          A.Post, unitP, D.PostRedirect
          A.Put, unitP, D.Created ]

    let private handlers =
        [ H.OK, defaultH 200 "OK"
          H.Created, defaultH 201 "Created"
          H.Options, defaultH 201 "Options"
          H.Accepted, defaultH 202 "Accepted"
          H.NoContent, defaultH 204 "No Content"
          H.MovedPermanently, defaultH 301 "Moved Permanently"
          H.SeeOther, defaultH 303 "See Other"
          H.NotModified, defaultH 304 "Not Modified"
          H.MovedTemporarily, defaultH 307 "Moved Temporarily"
          H.MultipleRepresentations, defaultH 310 "Multiple Representations"
          H.Malformed, defaultH 400 "Bad Request"
          H.Unauthorized, defaultH 401 "Unauthorized"
          H.Forbidden, defaultH 403 "Forbidden"
          H.NotFound, defaultH 404 "Not Found"
          H.MethodNotAllowed, defaultH 405 "Method Not Allowed"
          H.NotAcceptable, defaultH 406 "Not Acceptable"
          H.Conflict, defaultH 409 "Conflict"
          H.Gone, defaultH 410 "Gone"
          H.PreconditionFailed, defaultH 412 "Precondition Failed"
          H.RequestEntityTooLarge, defaultH 413 "Request Entity Too Large"
          H.UriTooLong, defaultH 414 "URI Too Long"
          H.UnsupportedMediaType, defaultH 415 "Unsupported Media Type"
          H.UnprocessableEntity, defaultH 422 "Unprocessable Entity"
          H.Exception, defaultH 500 "Internal Server Error"
          H.NotImplemented, defaultH 501 "Not Implemented"
          H.UnknownMethod, defaultH 501 "Unknown Method"
          H.ServiceUnavailable, defaultH 503 "Service Unavailable" ]
    
    let private internalDecisions =
        [ D.AcceptCharsetExists, headerExists "Accept-Charset", (D.CharSetAvailable, D.AcceptEncodingExists)
          D.AcceptEncodingExists, headerExists "Accept-Encoding", (D.EncodingAvailable, D.Processable)
          D.AcceptExists, headerExists "Accept", (D.MediaTypeAvailable, D.AcceptLanguageExists)
          D.AcceptLanguageExists, headerExists "Accept-Language", (D.LanguageAvailable, D.AcceptCharsetExists)
          D.IfMatchExists, headerExists "If-Match", (D.IfMatchStar, D.IfUnmodifiedSinceExists)
          D.IfMatchStar, trueP, (D.IfUnmodifiedSinceExists, D.ETagMatchesIf) // replace
          D.IfMatchStarExistsForMissing, headerExists "If-Match", (H.PreconditionFailed, D.MethodPut)
          D.IfModifiedSinceExists, headerExists "If-Modified-Since", (D.IfModifiedSinceValidDate, D.MethodDelete)
          D.IfModifiedSinceValidDate, trueP, (D.ModifiedSince, D.MethodDelete) // replace
          D.IfNoneMatch, trueP, (H.NotModified, H.PreconditionFailed) // replace
          D.IfNoneMatchExists, headerExists "If-None-Match", (D.IfNoneMatchStar, D.IfModifiedSinceExists)
          D.IfNoneMatchStar, trueP, (D.IfNoneMatch, D.ETagMatchesIfNone) // replace
          D.IfUnmodifiedSinceExists, headerExists "If-Unmodified-Since", (D.IfUnmodifiedSinceValidDate, D.IfNoneMatchExists)
          D.IfUnmodifiedSinceValidDate, trueP, (D.UnmodifiedSince, D.IfNoneMatchExists) // replace
          D.MethodDelete, isMethod DELETE, (A.Delete, D.MethodPatch)
          D.MethodOptions, isMethod OPTIONS, (H.Options, D.AcceptExists)
          D.MethodPatch, isMethod PATCH, (A.Patch, D.PostToExisting)
          D.MethodPut, isMethod PUT, (D.PutToDifferentUri, D.Existed)
          D.PostToGone, isMethod POST, (D.CanPostToGone, H.Gone)
          D.PostToExisting, isMethod POST, (A.Post, D.PutToExisting)
          D.PostToMissing, isMethod POST, (D.CanPostToMissing, H.NotFound)
          D.PutToExisting, isMethod PUT, (D.Conflict, D.MultipleRepresentations) ]

    let private publicDecisions =
        [ D.Allowed, trueP, (D.ContentTypeValid, H.Forbidden)
          D.Authorized, trueP, (D.Allowed, H.Unauthorized)
          D.CanPostToGone, falseP, (A.Post, H.Gone)
          D.CanPostToMissing, trueP, (A.Post, H.NotFound)
          D.CanPutToMissing, trueP, (D.Conflict, H.NotImplemented)
          D.CharSetAvailable, trueP, (D.AcceptEncodingExists, H.NotAcceptable) // replace
          D.Conflict, falseP, (H.Conflict, A.Put)
          D.ContentTypeKnown, trueP, (D.ValidEntityLength, H.UnsupportedMediaType)
          D.ContentTypeValid, trueP, (D.ContentTypeKnown, H.NotImplemented)
          D.Created, trueP, (H.Created, D.RespondWithEntity)
          D.Deleted, trueP, (D.RespondWithEntity, H.Accepted)
          D.EncodingAvailable, trueP, (D.Processable, H.NotAcceptable) // replace
          D.ETagMatchesIf, trueP, (D.IfUnmodifiedSinceExists, H.PreconditionFailed) // replace
          D.ETagMatchesIfNone, trueP, (D.IfNoneMatch, D.IfModifiedSinceExists) // replace
          D.Existed, falseP, (D.MovedPermanently, D.PostToMissing)
          D.Exists, trueP, (D.IfMatchExists, D.IfMatchStarExistsForMissing)
          D.MethodKnown, trueP, (D.UriTooLong, H.UnknownMethod) // replace
          D.LanguageAvailable, trueP, (D.AcceptCharsetExists, H.NotAcceptable) // replace
          D.Malformed, falseP, (H.Malformed, D.Authorized)
          D.MediaTypeAvailable, trueP, (D.AcceptLanguageExists, H.NotAcceptable) // replace
          D.MethodAllowed, trueP, (D.Malformed, H.MethodNotAllowed) // replace
          D.ModifiedSince, trueP, (D.MethodDelete, H.NotModified) // replace
          D.MovedPermanently, falseP, (H.MovedPermanently, D.MovedTemporarily)
          D.MovedTemporarily, falseP, (H.MovedTemporarily, D.PostToGone)
          D.MultipleRepresentations, falseP, (H.MultipleRepresentations, H.OK)
          D.PostRedirect, falseP, (H.SeeOther, D.Created)
          D.Processable, trueP, (D.Exists, H.UnprocessableEntity)
          D.PutToDifferentUri, falseP, (H.MovedPermanently, D.CanPutToMissing)
          D.RespondWithEntity, trueP, (D.MultipleRepresentations, H.NoContent)
          D.ServiceAvailable, trueP, (D.MethodKnown, H.ServiceUnavailable)
          D.UnmodifiedSince, trueP, (H.PreconditionFailed, D.IfNoneMatchExists) // replace
          D.UriTooLong, falseP, (H.UriTooLong, D.MethodAllowed) 
          D.ValidEntityLength, trueP, (D.MethodOptions, H.RequestEntityTooLarge) ]

    // Graph

    let private actionNodesFrom x r =
        x |> List.map (fun (n, f, next) -> n, Action (getOrElse action n f r, next))

    let private handlerNodesFrom x r =
        x |> List.map (fun (n, h) -> n, Handler (getOrElse handler n h r))

    let private internalDecisionNodesFrom x r =
        x |> List.map (fun (n, f, choices) -> n, Decision (f, choices))

    let private publicDecisionNodesFrom x r =
        x |> List.map (fun (n, f, choices) -> n, Decision (getOrElse decision n f r, choices))

    let private graphFrom r =
        [ actionNodesFrom actions
          handlerNodesFrom handlers
          internalDecisionNodesFrom internalDecisions
          publicDecisionNodesFrom publicDecisions ]
        |> Seq.map (fun x -> x r)
        |> Seq.concat
        |> Map.ofSeq

    // Execution

    let rec private execute graph =
        let rec execute node =
            frost {
                match Map.find node graph with
                | Action (f, next) ->
                    do! f
                    //printfn "action: %s" node
                    return! execute next
                | Decision (f, choices) ->
                    let! p = f
                    //printfn "decision: %s = %b" node p
                    return! execute ((p |> function | true -> fst | _ -> snd) choices)
                | Handler f ->
                    //printfn "handler: %s" node
                    return! f }

        execute D.ServiceAvailable

    // Temporary Visualisation 

    let visualiseResource resource =
        let def = defFrom resource
        let graph = graphFrom def

        let config =
            [ "fontname=Helvetica" ]
            |> String.concat "\n"

        let nodes =
            graph
            |> Map.toList
            |> List.map (fun (name, node) ->
                    match node with
                    | Action _ -> sprintf "%s [shape=circle;fontname=Helvetica]" name
                    | Decision _ -> sprintf "%s [shape=diamond;fontname=Helvetica]" name
                    | Handler _ -> sprintf "%s [shape=box;fontname=Helvetica]" name)
            |> String.concat "\n"
            
        let edges =
            graph 
            |> Map.toList
            |> List.map (fun (name, node) -> 
                match node with
                | Action (_, next) -> Some [ sprintf "%s -> %s [color=blue]" name next ]
                | Decision (_, choices) -> Some [ sprintf "%s -> %s [color=green]" name (fst choices)
                                                  sprintf "%s -> %s [color=red]" name (snd choices) ]
                | _ -> None)
            |> List.choose id
            |> List.concat
            |> String.concat "\n"
            
        sprintf "strict digraph Frost {\n%s\n%s\n%s\n}" config nodes edges               

    // Compilation

    let compileResource resource : FrostApp =
        let def = defFrom resource
        let graph = graphFrom def
        
        frost {
            do! Some def => Resource.Definition
            let! rep = execute graph

            printfn "Rep: %A" rep

            return () } |> compile
