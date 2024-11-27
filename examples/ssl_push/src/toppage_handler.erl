%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(toppage_handler).

-export([init/2]).

init(Req0, Opts) ->
	cowboy_req:push("/static/style.css", #{<<"accept">> => <<"text/css">>}, Req0),
	Req = cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/html">>
	}, <<"<html>
<head>
<link href=\"/static/style.css\" rel=\"stylesheet\" />
</head>
<body>
<h1>Hello world!</h1>
</body>
</html>">>, Req0),
	{ok, Req, Opts}.
