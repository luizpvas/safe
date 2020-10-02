Http.get "/posts", req ->
	Http.json (select * from posts)
