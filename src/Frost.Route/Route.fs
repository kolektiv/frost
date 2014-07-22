namespace Frost.Route

open FSharpx
open FSharpx.Lens.Operators
open Frost.Core
open Frost.Core.Operators


[<AutoOpen>]
module Types =

    type FrostRoutingTrie =
        { Root: FrostRoutingNode }

        static member empty =
            { Root =
                { App = None
                  Children = List.empty
                  Key = ""
                  Recognizer = Ignore "" } }

        static member root =
            { Get = fun x -> x.Root
              Set = fun r x -> { x with Root = r } }

    and FrostRoutingNode =
        { App: FrostApp option
          Children: FrostRoutingNode list
          Key: string
          Recognizer: FrostRoutingRecognizer }

        static member app =
            { Get = fun x -> x.App
              Set = fun a x -> { x with App = a } }
         
        static member children =
            { Get = fun x -> x.Children
              Set = fun c x -> { x with Children = c } }

    and FrostRoutingRecognizer =
        | Ignore of string
        | Capture of string

    type FrostRoutingRegistration =
        | Registration of string list * FrostApp


[<AutoOpen>]
module Lenses =

    [<RequireQualifiedAccess>]
    module Routing =

        let private opt = Lens.xmap (Option.get) (Some) Lens.id
        
        let Values = prop<Map<string, string>> "frost.routingData" >>| opt
        let Value key = Values >>| Lens.forMap key


[<AutoOpen>]
module Monad =

    // Monad Type

    type FrostRoutes = 
        FrostRoutingTrie -> unit * FrostRoutingTrie

    // Monad Builder

    type FrostRoutesBuilder () =

        member x.Return v : FrostRoutes = 
            tuple2 v

        member x.ReturnFrom f : FrostRoutes = 
            f

        member x.Bind (r, k) : FrostRoutes = 
            r >> fun (result, trie) -> (k result) trie

        member x.Combine (r1, r2) : FrostRoutes = 
            x.Bind (r1, fun () -> r2)

        member internal x.Update (r, update) = 
            x.Bind ((fun res -> (), update res), fun _ -> x.ReturnFrom r)

    // Monad Expression

    let frostRoutes = FrostRoutesBuilder ()


[<AutoOpen>]
module Functions =

    // Registration

    let private path (p: string) =
        p.Split '/'
        |> List.ofArray
        |> List.filter ((<>) "")

    let private registration p app =
        Registration (path p, app)

    let private recognizer (key: string) =
        match key.[0] with
        | ':' -> Capture (key.Substring (1))
        | _ -> Ignore (key)

    let private node key =
        { App = None
          Children = List.empty
          Key = key
          Recognizer = recognizer key }

    let add p app trie =
        let rec add registration root =
            match registration with
            | Registration (h :: t, app) ->
                match List.tryFindIndex (fun x -> x.Key = h) root.Children with
                | Some i -> 
                    Lens.update 
                        (fun x -> add (Registration (t, app)) x) 
                        (FrostRoutingNode.children >>| Lens.forList i) 
                        root
                | _ ->
                    Lens.update 
                        (fun x -> x @ [ add (Registration (t, app)) (node h) ])
                        (FrostRoutingNode.children) 
                        root
            | Registration (_, app) ->
                Lens.set (Some app) root FrostRoutingNode.app

        Lens.update (add (registration p app)) FrostRoutingTrie.root trie

    // Search

    let recognize r data value =
        match r with
        | Capture x -> true, Map.add x value data
        | Ignore x when x = value -> true, data
        | _ -> false, data

    let search p trie =
        let rec search data path root =
            match path with
            | h :: t -> 
                root.Children
                |> List.tryPick (fun x ->
                      match recognize x.Recognizer data h with
                      | true, map -> Some (x, map)
                      | _ -> None)
                |> fun x ->
                    match x with
                    | Some (child, map) -> search map t child
                    | _ -> None
            | _ -> 
                root.App |> Option.map (fun x -> x, data)

        search Map.empty (path p) trie.Root

    // Compilation

    let compileRoutes (routes: FrostRoutes) : FrostApp =
        let trie = snd << routes <| FrostRoutingTrie.empty

        frost {
            let! path = !! Request.Path

            match search path trie with
            | Some (app, data) ->
                do! data => Routing.Values 
                do! !! Lens.id >>= fun e -> !!! (app e) >>= fun e -> e => Lens.id
            | _ -> 
                return () } |> compile