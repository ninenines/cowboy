{application, 'cowboy', [
	{description, "Small, fast, modern HTTP server."},
	{vsn, "2.0.0-pre.10"},
	{modules, ['cowboy','cowboy_app','cowboy_bstr','cowboy_clear','cowboy_clock','cowboy_compress_h','cowboy_constraints','cowboy_handler','cowboy_http','cowboy_http2','cowboy_iolists','cowboy_loop','cowboy_middleware','cowboy_req','cowboy_rest','cowboy_router','cowboy_static','cowboy_stream','cowboy_stream_h','cowboy_sub_protocol','cowboy_sup','cowboy_tls','cowboy_websocket']},
	{registered, [cowboy_sup,cowboy_clock]},
	{applications, [kernel,stdlib,crypto,cowlib,ranch]},
	{mod, {cowboy_app, []}},
	{env, []}
]}.