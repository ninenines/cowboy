-module(cowboy_regexp_router).

-export([compile/1]).
-export([execute/2]).

-compile(export_all).

compile(Rules) ->
	[compile_(R) || R <- Rules].

compile_({HostRe, Paths}) ->
	compile_({HostRe, [], Paths});
compile_({HostRe, Constraints, Paths}) ->
	{ok, CompiledRe} = re_utils:compile(HostRe),
	{CompiledRe, Constraints, [compile_path(P) || P <- Paths]}.

compile_path({PathRe, Module, Opts}) ->
	compile_path({PathRe, [], Module, Opts});
compile_path({PathRe, Constraints, Module, Opts}) ->
	{ok, CompiledRe} = re_utils:compile(PathRe),
	{CompiledRe, Constraints, Module, Opts}.

execute(Req, Env) ->
	{_, Dispatch} = lists:keyfind(regexp_dispatch, 1, Env),
	[Host, Path] = cowboy_req:get([host, path], Req),
	case match(Dispatch, Host, Path) of
		{ok, Handler, HandlerOpts, Bindings} ->
			Req2 = cowboy_req:set([{bindings, Bindings}], Req),
			{ok, Req2, [{handler, Handler}, {handler_opts, HandlerOpts}|Env]};
		{error, notfound, host} ->
			{error, 400, Req};
		{error, badrequest, path} ->
			{error, 400, Req};
		{error, notfound, path} ->
			{error, 404, Req}
	end.

match([], _Host, _Path) ->
	{error, notfound, host};
match([{HostRe, Constraints, PathsRe}|Tail], Host, Path) ->
	case re_utils:captures(Host, HostRe) of
		{match, Bindings} ->
			case check_constraints(Constraints, Bindings) of
				{ok, Bindings2} ->
					match_path(PathsRe, Path, Bindings2);
				nomatch ->
					match(Tail, Host, Path)
			end;
		nomatch ->
			match(Tail, Host, Path)
	end.

match_path([], _Path, _Bindings) ->
	{error, notfound, path};
match_path([{PathRe, Constraints, Handler, Opts}|Tail], Path, Bindings) ->
	case re_utils:captures(Path, PathRe) of
		{match, PathBindings} ->
			case check_constraints(Constraints, PathBindings) of
				{ok, PathBindings2} ->
					{ok, Handler, Opts, PathBindings2};
				nomatch ->
					match_path(Tail, Path, Bindings)
			end;
		nomatch ->
			match_path(Tail, Path, Bindings)
	end.

check_constraints([], Bindings) ->
	{ok, Bindings};
check_constraints([Constraint|Tail], Bindings) ->
	Name = element(1, Constraint),
	case lists:keyfind(Name, 1, Bindings) of
		false ->
			check_constraints(Tail, Bindings);
		{_, Value} ->
			case check_constraint(Constraint, Value) of
				true ->
					check_constraints(Tail, Bindings);
				{true, Value2} ->
					Bindings2 = lists:keyreplace(Name, 1, Bindings, {Name, Value2}),
					check_constraints(Tail, Bindings2);
				false ->
					nomatch
			end
	end.

check_constraint({_, int}, Value) ->
	try {true, list_to_integer(binary_to_list(Value))}
	catch _:_ -> false
	end;
check_constraint({_, function, Fun}, Value) ->
	Fun(Value).
