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

        static member fromString =
            function | "DELETE" -> DELETE 
                     | "HEAD" -> HEAD 
                     | "GET" -> GET 
                     | "OPTIONS" -> OPTIONS
                     | "PATCH" -> PATCH 
                     | "POST" -> POST 
                     | "PUT" -> PUT 
                     | "TRACE" -> TRACE
                     | x -> Custom x 

        static member toString =
            function | DELETE -> "DELETE" 
                     | HEAD -> "HEAD" 
                     | GET -> "GET" 
                     | OPTIONS -> "OPTIONS"
                     | PATCH -> "PATCH" 
                     | POST -> "POST" 
                     | PUT -> "PUT"  
                     | TRACE -> "TRACE"
                     | Custom x -> x

        static member map = 
            Method.fromString, Method.toString


[<AutoOpen>]
module Protocol =

    type Protocol =
        | HTTP of float 
        | Custom of string

        static member fromString =
            function | "HTTP/1.0" -> HTTP 1.0 
                     | "HTTP/1.1" -> HTTP 1.1 
                     | x -> Custom x

        static member toString =
            function | HTTP x -> sprintf "HTTP/%f" x 
                     | Custom x -> x

        static member map =
            Protocol.fromString, Protocol.toString


[<AutoOpen>]
module Scheme =

    type Scheme =
        | HTTP 
        | HTTPS 
        | Custom of string

        static member fromString =
            function | "http" -> HTTP 
                     | "https" -> HTTPS 
                     | x -> Custom x

        static member toString =
            function | HTTP -> "http" 
                     | HTTPS -> "https" 
                     | Custom x -> x

        static member map =
            Scheme.fromString, Scheme.toString
