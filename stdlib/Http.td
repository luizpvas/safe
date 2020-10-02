module Http exposing (..)

import Http.Request
import Http.Response
import Http.Route
import Encoding.Json


-- Registers the route handler.
get : String -> (Request -> Response) -> Future Route
get path handler =
	Debug.todo "implement this"


-- Wraps the given text in a "text/plain" response to a HTTP request.
textPlain : String -> Response
textPlain body =
	Response.TextPlain body


-- Encodes and wraps the given value in a JSON response. The `data`
-- argument must be serializable.
json : a -> Response
json data =
	Response.Json (Json.encode data)
