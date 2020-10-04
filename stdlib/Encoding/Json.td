module Encoding.Json exposing (..)


interface Encodable a =
    encode : a -> Value


interface Decodable a =
    decode : Value -> Result String a