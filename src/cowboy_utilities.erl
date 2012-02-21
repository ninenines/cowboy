%% Copyright (c) 2012, Roberto Ostinelli <roberto@ostinelli.net>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

%% @doc Mixed common utilities functions.
-module(cowboy_utilities).

-export([get_value/2, get_value/3]).

-include_lib("eunit/include/eunit.hrl").


%% @doc Same as proplists:get_value/2,3 when used on list of tuples. Just faster.
-spec get_value(Key::term(), List::[term()]) -> Value::term() | undefined.
-spec get_value(Key::term(), List::[term()], Default::term()) -> Value::term() | undefined.
get_value(Key, List) ->
	get_value(Key, List, undefined).
get_value(Key, List, Default) ->
	case lists:keyfind(Key, 1, List) of
		false->
			case lists:member(Key, List) of
				true -> true;
				false -> Default
			end;
		{Key, Value}-> Value
	end.

%% Tests.

-ifdef(TEST).

get_value_test_() ->
	[
		?_assertEqual(1, get_value(a, [{a, 1}, {b, 2}, {c, 3}])),
		?_assertEqual(2, get_value(b, [{a, 1}, {b, 2}])),
		?_assertEqual(3, get_value(c, [{c, 3}])),
		?_assertEqual(undefined, get_value(a, [])),
		?_assertEqual(undefined, get_value(d, [{a, 1}, {b, 2}, {c, 3}])),
		?_assertEqual(1, get_value(a, [{a, 1}, {b, 2}, {c, 3}], default)),
		?_assertEqual(2, get_value(b, [{a, 1}, {b, 2}], default)),
		?_assertEqual(3, get_value(c, [{c, 3}], default)),
		?_assertEqual(default, get_value(a, [], default)),
		?_assertEqual(default, get_value(d, [{a, 1}, {b, 2}, {c, 3}], default)),
		?_assertEqual(true, get_value(included, [included])),
		?_assertEqual(undefined, get_value(not_included, [included])),
		?_assertEqual(undefined, get_value(not_included, [])),
		?_assertEqual(false, get_value(not_included, [included], false)),
		?_assertEqual(false, get_value(not_included, [], false))
	].

-endif.
