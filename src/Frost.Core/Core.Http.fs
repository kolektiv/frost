namespace Frost.Core

open FSharpx


[<AutoOpen>]
module Method =

    type Method =
        | DELETE 
        | HEAD 
        | GET 
        | OPTIONS 
        | PATCH 
        | POST 
        | PUT 
        | TRACE 
        | Custom of string

    let internal fromString =
        function | "DELETE" -> DELETE 
                 | "HEAD" -> HEAD 
                 | "GET" -> GET 
                 | "OPTIONS" -> OPTIONS
                 | "PATCH" -> PATCH 
                 | "POST" -> POST 
                 | "PUT" -> PUT 
                 | "TRACE" -> TRACE
                 | x -> Custom x 

    let internal toString =
        function | DELETE -> "DELETE" 
                 | HEAD -> "HEAD" 
                 | GET -> "GET" 
                 | OPTIONS -> "OPTIONS"
                 | PATCH -> "PATCH" 
                 | POST -> "POST" 
                 | PUT -> "PUT"  
                 | TRACE -> "TRACE"
                 | Custom x -> x

    let internal map = 
        fromString, toString


[<AutoOpen>]
module Protocol =

    type Protocol =
        | HTTP of float 
        | Custom of string

    let internal fromString =
        function | "HTTP/1.0" -> HTTP 1.0 
                 | "HTTP/1.1" -> HTTP 1.1 
                 | x -> Custom x
    
    let internal toString =
        function | HTTP x -> sprintf "HTTP/%f" x 
                 | Custom x -> x

    let map =
        fromString, toString


[<AutoOpen>]
module QueryString =

    type QueryString =
        Map<string, string>
    
    let internal fromString (s: string) =
        match s with
        | "" -> 
            Map.empty
        | s ->
            s.Split [| '&' |]
            |> Array.map (fun x -> x.Split [| '=' |])
            |> Array.map (fun x -> x.[0], x.[1])
            |> Map.ofArray

    let internal toString (q: QueryString) =
        Map.toArray q
        |> Array.map (fun x -> sprintf "%s=%s" (fst x) (snd x))
        |> String.concat "&"

    let internal map =
        fromString, toString


[<AutoOpen>]
module Scheme =

    type Scheme =
        | HTTP 
        | HTTPS 
        | Custom of string

    let internal fromString =
        function | "http" -> HTTP 
                 | "https" -> HTTPS 
                 | x -> Custom x

    let internal toString =
        function | HTTP -> "http" 
                 | HTTPS -> "https" 
                 | Custom x -> x

    let internal map =
        fromString, toString
