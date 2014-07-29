module Frost.Examples.VersionedStorage.Http

open Frost.Core
open Frost.Core.Operators
open Frost.Resource
open Core

// Request

let meth = get Request.Method @>> "method"
let path = get Request.Path @>> "path"
let body = get Request.Body @>> "body"
let commit = get (Request.Query "commit") @>> "commit"

// Files

let file = (<||) read <!> Frost.tuple2 path commit @>> "file"

// Actions

let fileCreate = (<||) create <!> Frost.tuple2 path body
let fileDelete = delete <!> path

// Decisions

let fileExists = Option.isSome <!> file
let fileShow = Option.get <!> file

// Resources

let files =
    frostResource {
        allowedMethods [ DELETE; GET; POST ]
        exists fileExists
        doPost fileCreate
        doDelete fileDelete
        handleOk fileShow } |> compileResource

// Pipes

let poweredBy = Response.Header "X-PoweredBy" <-- Some [ "Frost" ]
let version = Response.Header "X-Version" <-- Some [ "0.1" ]

// Filters

let redirectCommit path =
    Frost.seq [ Response.StatusCode <-- Some 302
                Response.ReasonPhrase <-- Some "Moved Temporarily"
                Response.Header "Location" <-- Some [ sprintf "%s?commit=%s" path (sha ()) ] ]

let requireCommit =
    frost {
        let! meth = meth
        let! commit = commit

        match meth, commit with
        | GET, None ->
            do! path >>= redirectCommit
            return false
        | _ -> 
            return true }

// Main

let main =
    poweredBy
    >-> version
    >-> requireCommit 
    >?> files 
    |> toFunc
