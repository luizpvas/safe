module Encoding.Json exposing (..)


interface Encodable a =
    toJson : a -> Value


interface Decodable a =
    fromJson : Value -> Result String a