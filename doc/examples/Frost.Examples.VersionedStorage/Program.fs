module Frost.Examples.VersionedStorage.Program

open System
open Microsoft.Owin.Hosting
open Http

// App

type VersionedStorage () =
    member x.Configuration () = app

// Main

[<EntryPoint>]
let main _ = 
    WebApp.Start<VersionedStorage> "http://localhost:8000"
    |> fun x -> Console.ReadLine () |> ignore; x
    |> fun x -> x.Dispose (); 0
