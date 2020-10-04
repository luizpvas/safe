module Database.Query exposing (..)


type alias Query =
    { select : String
    , where : String
    }