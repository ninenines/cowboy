-module(cowboy_util).

-export([get_ranges/2, get_ranges/3]).
-export([parse_range_header/1, parse_range_header/2]).

get_ranges(Header, Name) ->
  get_ranges(Header, Name, 0).

get_ranges(Header, Name, Length) ->
  NameStr = binary_to_list(Name),
  lists:foldr(fun(E, A) ->
    case E of
      {NameStr, bad} ->
        [bad | A];
      {NameStr, Start, End} ->
        [{Start, End} | A];
      _ ->
        A
    end
  end, [], parse_range_header(Header, Length)).

parse_range_header(Header) ->
  parse_range_header(Header, 0).

parse_range_header(Header, Length) when is_binary(Header), is_integer(Length) ->
  MayBeRanges = string:tokens(binary_to_list(Header), "\s;"),
  Ranges = lists:map(fun(E) ->
    case re:run(E, "^([a-z]+)=([0-9-]+)$", [{capture, all, list}]) of
      {match, [_, Name, Value]} ->
        case parse_range(Value) of
          bad ->
            {Name, bad};
          {Start, End} ->
            Start1 = wrap_negative(Start, Length),
            End1 = wrap_negative(End, Length),
            {Name, Start1, End1}
        end;
      _ -> bad
    end
  end, MayBeRanges),
  Ranges;

parse_range_header(undefined, _) ->
  undefined.

% X-Y
parse_range(Data) ->
  case string:to_integer(Data) of
    {error, _} ->
      bad;
    {Start, ""} ->
      {Start, Start};
    {Start, "-"} ->
      {Start, -1};
    {Start, "-" ++ Data1} ->
      case string:to_integer(Data1) of
        {End, ""} ->
          {Start, End};
        _ ->
          bad
      end
  end.

% X < 0 ? X + Y : X
wrap_negative(X, L) when X < 0 -> X + L;
wrap_negative(X, _L) -> X.
