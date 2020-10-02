module Http.Response exposing (..)

type Response
	= TextPlain String
	| Json String
	| Html String
