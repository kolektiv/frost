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

let file = (<||) retrieve <!> Frost.tuple2 path commit @>> "file"

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
        handleOk fileShow }

// Powered By

let poweredBy =
    frost {
        do! Response.Header "X-PoweredBy" <-- Some [ "Frost" ]
        return true }

// Redirect

let redirect =
    frost {
        let! meth = meth
        let! path = path
        let! commit = commit

        match meth, commit with
        | GET, None ->
            do! Response.StatusCode <-- Some 302
            do! Response.ReasonPhrase <-- Some "Moved Temporarily"
            do! Response.Header "Location" <-- Some [ sprintf "%s?commit=%s" path (sha ()) ]
            return false
        | _ ->
            return true }

// Main

let main =
    poweredBy
    >-> redirect 
    >-> compileResource files 
    |> toFunc
