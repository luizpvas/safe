module Database.Table exposing (Table)

import Database.Column as Column exposing (Column)
import Json.Decode as Decode exposing (Decoder)


type alias Table =
    { name : String
    , columns : List Column
    }


mysqlDecoder : Decoder Table
mysqlDecoder =
    Decode.map2 Table
        (Decode.field "name" Decode.string)
        (Decode.field "columns" (Decode.list Column.mysqlDecoder))
