module Frost.Examples.VersionedStorage.Http

open Frost.Core
open Frost.Core.Operators
open Frost.Resource
open FSharpx
open Core

// Helpers

let meth = get Request.Method
let path = get Request.Path
let body = get Request.Body
let commit = get (Request.Query "commit")

// Actions

let fileCreate =
    frost {
        let! path = path
        let! body = body

        do create path body }

let fileDelete =
    frost {
        let! path = path

        do delete path }

// Decisions

let fileExists = 
    frost {
        let! path = path
        let! commit = commit

        match retrieve path commit with
        | Some data ->
            do! key "app.data" <-- Some data
            return true
        | _ ->
            return false }

// Handlers

let fileShow =
    frost {
        return! Option.get <!> get (key "data") }

// Resources

let files =
    frostResource {
        allowedMethods [ DELETE; GET; POST ]
        exists fileExists

        doPost fileCreate
        doDelete fileDelete

        handleOk fileShow }

// Redirect

let redirect =
    frost {
        let! meth = meth
        let! path = path
        let! commit = commit

        match meth, commit with
        | GET, None ->
            let sha = sha ()

            do! Response.StatusCode <-- Some 302
            do! Response.ReasonPhrase <-- Some "Moved Temporarily"
            do! Response.Header "Location" <-- Some [ sprintf "%s?commit=%s" path sha ]

            return false
        | _ ->
            return true }

// App

let app =
    redirect >-> compileResource files |> toFunc