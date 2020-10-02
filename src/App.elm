module Main exposing (App)


type alias App =
    { name : String
    , absoluteRootPath : String
    , filesPathsFromRoot : List String
    }
