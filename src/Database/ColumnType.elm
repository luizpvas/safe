module Database.ColumnType exposing (ColumnType(..), mysqlDecoder)

import Json.Decode as Decode exposing (Decoder)


type ColumnType
    = TinyInteger
    | Integer
    | BigInteger
    | Varchar Int
    | Text


mysqlDecoder : Decoder ColumnType
mysqlDecoder =
    Decode.string
        |> Decode.andThen
            (\typeName ->
                case typeName of
                    "tinyint" ->
                        Decode.succeed TinyInteger

                    _ ->
                        Decode.fail ("unrecorginzed colum type: " ++ typeName)
            )
