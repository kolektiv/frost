namespace Frost.Route


[<AutoOpen>]
module RoutesOperations =

    type FrostRoutesBuilder with

        // Registration

        [<CustomOperation ("register", MaintainsVariableSpaceUsingBind = true)>]
        member x.Register (r, path, app) = x.Update (r, add path app)
    
        // Utility

        [<CustomOperation ("including", MaintainsVariableSpaceUsingBind = true)>]
        member x.Including (r, routes) = x.Combine (r, routes)
