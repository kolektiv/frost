open System
open System.IO
open System.Text
open Frost.Core
open Frost.Core.Operators
open Frost.Resource
open Frost.Route
open FSharpx
open FSharpx.Lens.Operators
open Microsoft.Owin.Hosting

// Store 

let mutable store : Map<string, string> = Map.empty

let add k v = async { do store <- Map.add k v store }
let delete k = async { do store <- Map.remove k store }
let update k v = async { do store <- Map.add k v (Map.remove k store) }
let tryGet k = async { return Map.tryFind k store }
let list = async { return store |> Map.toList }

// Lenses 

[<RequireQualifiedAccess>]
module App =

    let Key = key<string> "key" >>| required
    let Value = key<string> "value" >>| required
    let NewValue = key<string> "newValue" >>| required
    let ExistingValue = key<string> "existingValue" >>| required

// Actions

let addKeyValue =
    frost {
        let! key = get App.Key
        let! value = get App.Value

        do! Frost.async (add key value) }

let removeKey =
    frost {
        let! key = get App.Key

        do! Frost.async (delete key) }

let updateValue =
    frost {
        let! key = get App.Key
        let! value = get App.NewValue

        do! Frost.async (update key value) }

// Helpers

let private toString (s: Stream) =
    use reader = new StreamReader (s, Encoding.UTF8)
    reader.ReadToEnd ()

let private toPair s =
    toString s 
    |> fun x -> x.Split '|'
    |> fun xs -> xs.[0], xs.[1]

// Decisions

let dataMalformed =
    frost {
        let! meth = get Request.Method

        match meth with
        | POST ->
            let! key, value = toPair <!> get Request.Body

            do! App.Key <-- key
            do! App.Value <-- value

            return false 
        | PUT ->
            let! value = toString <!> get Request.Body

            do! App.NewValue <-- value

            return false
        | _ ->
            return false }

let keyExists =
    frost {
        let! key = Option.get <!> get (Routing.Value "key")
        let! value = Frost.async (tryGet key)        

        match value with
        | Some value ->
            do! App.Key <-- key
            do! App.ExistingValue <-- value

            return true
        | _ ->
            return false }

// Handlers

let showValue = box <!> get App.ExistingValue
let showKeyValues = box <!> Frost.async list

// Resources

let negotiation =
    frostResource {
        availableMediaTypes [ "application/json" ] }

let key =
    frostResource {
        including negotiation

        allowedMethods [ GET; DELETE; PUT ]
        doDelete removeKey
        doPut updateValue
        exists keyExists
        malformed dataMalformed
        handleOk showValue }

let keys =
    frostResource {
        including negotiation

        allowedMethods [ GET; POST ]
        doPost addKeyValue
        malformed dataMalformed
        handleOk showKeyValues }

// Routes

let routes =
    frostRoutes {
        resource "/" keys
        resource "/:key" key }

// App

type KeyValueStore () =
    member x.Configuration () =
        toFunc << compileRoutes <| routes


[<EntryPoint>]
let main _ =

    let app = WebApp.Start<KeyValueStore> "http://localhost:8000"

    System.Console.ReadLine () |> ignore
    app.Dispose ()

    0