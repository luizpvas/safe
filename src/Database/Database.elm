module Database.Database exposing (Database)

import Database.Driver exposing (Driver)


type alias Database =
    { driver : Driver
    , name : String
    , host : String
    , port_ : Int
    }
