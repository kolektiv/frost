module Frost.Examples.VersionedStorage.Program

open System
open Microsoft.Owin.Hosting

// App

type VersionedStorage () =
    member x.Configuration () = Http.main

// Main

[<EntryPoint>]
let main _ = 
    WebApp.Start<VersionedStorage> "http://localhost:8000"
    |> fun x -> Console.ReadLine (), x
    |> fun (_, x) -> x.Dispose ()

    0
