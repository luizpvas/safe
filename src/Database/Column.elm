module Database.Column exposing (Column, mysqlDecoder)

import Database.ColumnType as ColumnType exposing (ColumnType)
import Json.Decode as Decode exposing (Decoder)


type alias Column =
    { name : String
    , type_ : ColumnType
    , null : Bool
    }


mysqlDecoder : Decoder Column
mysqlDecoder =
    Decode.map3 Column
        (Decode.field "name" Decode.string)
        (Decode.field "type" ColumnType.mysqlDecoder)
        (Decode.field "null" Decode.bool)
