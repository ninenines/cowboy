%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @copyright (C) Erlware, LLC.
%%% @copyright (C) 2009, Gordon Guthrie
%%% @doc
-module(erlmarkdown).

-export([conv/1,
         conv_utf8/1,
         conv_file/2]).

-define(SPACE, 32).
-define(TAB,    9).
-define(LF,    10).
-define(CR,    13).
-define(NBSP, 160).
-define(AMP, $&, $a, $m, $p, $;).
-define(COPY, $&, $c, $o, $p, $y, $;).

%%============================================================================
%% API
%%============================================================================

%% the lexer first lexes the input
%% make_lines does 2 passes:
%% * it chops the lexed strings into lines which it represents as a
%%   list of lists
%% * it then types the lines into the following:
%% * normal lines
%% * reference style links
%% * reference style images
%% * special line types
%%   - blank
%%   - SETEXT header lines
%%   - ATX header lines
%%   - blockquote
%%   - unordered lists
%%   - ordered lists
%%   - code blocks
%%   - horizontal rules
%% the parser then does its magic interpolating the references as appropriate
conv(String) ->
    Lex = lex(String),
    UntypedLines = make_lines(Lex),
    {TypedLines, Refs} = type_lines(UntypedLines),
    parse(TypedLines, Refs).

-spec conv_utf8(list()) -> list().
conv_utf8(Utf8) ->
    Str = xmerl_ucs:from_utf8(Utf8),
    Res = conv(Str),
    xmerl_ucs:to_utf8(Res).

conv_file(FileIn, FileOut) ->
    case file:open(FileIn, [read]) of
        {ok, Device} ->
            Input = get_all_lines(Device,[]),
            Output = conv(Input),
            write(FileOut, Output);
        _ ->
            error
    end.

%%============================================================================
%% Internal Functions
%%============================================================================

get_all_lines(Device, Accum) ->
    case io:get_line(Device,"") of
        eof  ->
            file:close(Device),
            Accum;
        Line ->
            get_all_lines(Device,Accum ++ Line)
    end.

write(File, Text) ->
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Text]),
            file:close(Id);
        _ ->
            error
    end.


%%
%% Parse the lines interpolating the references as appropriate
%%

parse(TypedLines, Refs) ->
    string:strip(p1(TypedLines, Refs, 0, []), both, $\n).

%% goes through the lines
%% Variable 'R' contains the References and 'I' is the indent level

%% Terminal clause
p1([], _R, _I, Acc) ->
    lists:flatten(lists:reverse(Acc));

%% Tags have the highest precedence...
p1([{tag, Tag} | T], R, I, Acc) ->
    case T of
        [] ->
            p1([], R, I, ["</p>", make_tag_str(Tag, R), "<p>" | Acc]);
        [{blank, _} | T2] ->
            p1(T2, R, I, [make_tag_str(Tag, R) | Acc]);
        _Other ->
            p1(T, R, I, [pad(I) ++ make_tag_str(Tag, R) | Acc])
    end;

p1([{blocktag, [{{{tag, open}, Type}, Tg}] = _Tag} | T], R, I, Acc) ->
    {Block, Rest} = grab_for_blockhtml(T, Type, []),
    Str = lists:flatten([Tg, "\n" | Block]),
    p1(Rest, R, I, [Str | Acc]);

%% blank lines/linefeeds are gobbled down
p1([{Type, _} | T], R, I, Acc)
  when Type == blank orelse Type == linefeed ->
    Rest = grab_empties(T),
    p1(Rest, R, I, [pad(I) ++ "\n" | Acc]);

%% two consecutive normal lines should be concatenated...
%% remembering the pad the second line with the indent...
p1([{normal, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, merge(P1, pad(I), P2)} | T], R, I, Acc);
%% as should a normal and linefeed

%% setext h1 is a look behind and it overrides blockquote and code...
p1([{normal, P}, {setext_h1, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h1>" ++ make_string(snip(P), R)
                  ++ "</h1>\n\n" | Acc]);
p1([{blockquote, P}, {setext_h1, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h1>" ++ make_string(snip(P), R)
                  ++ "</h1>\n\n" | Acc]);
p1([{{codeblock, P}, _}, {setext_h1, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h1>" ++ make_string(snip(P), R)
                  ++ "</h1>\n\n" | Acc]);
p1([{blockquote, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_string(snip(P), R)
                  ++ "</h2>\n\n" | Acc]);
p1([{{codeblock, P}, _}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_string(snip(P), R)
                  ++ "</h2>\n\n" | Acc]);

%% but a setext with no lookbehind is just rendered as a normal line,
%% so change its type and rethrow it
p1([{setext_h1, P} | T], R, I, Acc) ->
    p1([{normal, P} | T], R, I, Acc);

%% setext h2 might be a look behind
p1([{normal, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    P2 = string:strip(make_string(snip(P), R), both, ?SPACE),
    p1(T, R, I, [pad(I) ++ "<h2>" ++ P2 ++ "</h2>\n\n" | Acc]);

%% blockquotes swallow each other
%% replace the first blockquote mark with a space...
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, pad(I), [{{ws, sp}, " "} | P2])} | T], R, I, Acc);
%% blockquotes swallow normal
p1([{blockquote, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, pad(I + 1), P2)} | T], R, I, Acc);
%% blockquote
p1([{blockquote, P} | T], R, I, Acc) ->
    [{{md, gt}, _} | T1] = P,
    T2 = string:strip(make_string(T1, R)),
    p1(T, R, I,
       ["\n<blockquote>\n" ++ pad(I + 1) ++ "<p>" ++ T2 ++ "</p>\n</blockquote>" | Acc]);

%% one normal is just normal...
p1([{normal, P} | T], R, I, Acc) ->
    P2 = string:strip(make_string(snip(P), R), both, ?SPACE),
    p1(T, R, I, [pad(I) ++ "<p>" ++ P2 ++ "</p>\n" | Acc]);

%% atx headings
p1([{{h1, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h1>" ++ NewP ++ "</h1>\n\n" | Acc]);
p1([{{h2, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ NewP ++ "</h2>\n\n" | Acc]);
p1([{{h3, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h3>" ++ NewP ++ "</h3>\n\n" | Acc]);
p1([{{h4, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h4>" ++ NewP ++ "</h4>\n\n" | Acc]);
p1([{{h5, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h5>" ++ NewP ++ "</h5>\n\n" | Acc]);
p1([{{h6, P}, _} | T], R, I, Acc) ->
    NewP = string:strip(make_string(snip(P), R), right),
    p1(T, R, I,  [pad(I) ++ "<h6>" ++ NewP ++ "</h6>\n\n" | Acc]);

%% unordered lists swallow normal and codeblock lines
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ul, _P}, _} | _T] = List, R, I, Acc) ->
    {Rest, NewAcc} = parse_list(ul, List, R, I, [], false),
    p1(Rest, R, I,  [pad(I) ++ "<ul>\n" ++ NewAcc
                     ++ pad(I) ++ "</ul>\n" | Acc]);

%% ordered lists swallow normal and codeblock lines
p1([{{ol, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ol, _P}, _} | _T] = List, R, I, Acc) ->
    {Rest, NewAcc} = parse_list(ol, List, R, I, [], false),
    p1(Rest, R, I,  [pad(I) ++ "<ol>\n" ++ NewAcc
                     ++ pad(I) ++ "</ol>\n" | Acc]);

%% codeblock consumes any following empty lines
%% and other codeblocks
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], R, I, Acc) ->
    p1([{{codeblock, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{codeblock, P}, _} | T], R, I, Acc) ->
    Rest = grab_empties(T),
    p1(Rest, R, I,  ["<pre><code>" ++ make_string(snip(P), R)
                     ++ "\n</code></pre>\n\n" | Acc]);

%% horizontal rules
p1([{hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  ["<hr />" | Acc]);
%% h2_or_hr is greedy for normal lines
p1([{h2_or_hr, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, lists:flatten([P1 | P2])} | T], R, I, Acc);
%% the clause with a normal before an 'h2_or_hr' has already been
%% handled further up the tree, so this is a bona fide 'hr'...
p1([{h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  ["<hr />" | Acc]);

%% Now start pulling out inline refs etc, etc
p1([{inlineref, _P} | T], R, I, Acc) ->
    p1(T, R, I, Acc).

grab_for_blockhtml([], Type, Acc) ->
    {lists:reverse(["</" ++ Type ++ ">" | Acc]), []};
grab_for_blockhtml([{blocktag, [{{{tag, close}, Type}, Tg}]}
                    | T], Type,  Acc) ->
    {lists:reverse([Tg | Acc]), T};
grab_for_blockhtml([{blocktag, [{{{tag, _}, GrabType}, Tg}]}
                    | T], Type,  Acc) when GrabType =/= Type ->
    %% blocktags grabbed in a blocktag need a line ending pushed
    grab_for_blockhtml(T, Type, ["\n", Tg | Acc]);
grab_for_blockhtml([{tag, {{{tag, self_closing}, _Ty}, Tg}}
                    | T], Type, Acc) ->
    grab_for_blockhtml(T, Type, [Tg | Acc]);
grab_for_blockhtml([H | T], Type, Acc) ->
    {_Type, Content} = H,
    Str = make_plain_string(Content),
    grab_for_blockhtml(T, Type, [Str | Acc]).

grab_empties([{linefeed, _} | T]) -> grab_empties(T);
grab_empties([{blank, _} | T])    -> grab_empties(T);
grab_empties(List)                -> List.

merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {string, Pad} | P2]).

make_br(List) -> make_br1(lists:reverse(List)).

make_br1([{{lf, _}, _},
          {{ws, comp}, _} | T]) -> lists:reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _},
          {{ws, tab}, _} | T])  -> lists:reverse([{tags, " <br />\n"} | T]);
make_br1(List)                  -> lists:reverse(List).

pad(N) ->
    pad1(N, []).

pad1(0, Acc) ->
    Acc;
pad1(N, Acc)
  when N > 0 ->
    pad1(N - 1, ["  " | Acc]).

%% this is a bit messy because of the way that hard lines are treated...
%% If your li's have a blank line between them the item gets wrapped in a para,
%% if not, they don't
%% BUT if one item is <p> wrapped then the next is too
parse_list(_Type, [], _R, _I, A, _) ->
    {[], lists:reverse(A)};
parse_list(Type, [{{Type, P}, _} | T], R, I, A, Wrap) ->
    {Rest, NewP, NewWrap} = grab(T, R, [], Wrap),
    Li =
        case NewWrap of
            false ->
                Ret = parse([{normal, P}], R),
                %% need to strip off the extra <p></p>'s
                Ret2 = string:left(Ret, length(Ret) - 4),
                Ret3 = string:right(Ret2, length(Ret2) -3),
                Ret3 ++ "\n" ++ NewP ++ pad(I);
            true  ->
                string:strip(parse([{normal, P}], R), right, ?LF)
                    ++ NewP ++ pad(I)
        end,
    NewWrap2 =
        case T of
            []  ->
                false; % doesnt matter
            [H2 | _T2] ->
                case H2 of
                    {linefeed, _} ->
                        true;
                    _  ->
                        false
                end
        end,
    parse_list(Type, Rest, R, I, [pad(I) ++ "<li>"
                                  ++ string:strip(Li, right, ?LF)
                                  ++ "</li>\n" | A], NewWrap2);
parse_list(_Type, List, _R, _I, A, _) ->
    {List, lists:reverse(A)}.

%% grab grabs normals, double codeblocks, linefeeds and blanks
%% BUT stop grabbing if a normal if preceeded by a linefeed or blank
%% UNLESS the normal starts with white space :(
%% the third return parameter is 'true' if the 'li' should be
%% wrapped in '<p></p>' and false if it shouldn't
grab([{{codeblock, _}, S} | T] = List, R, Acc, W) ->
    case is_blockquote(S, T) of
        {{true, R1}, T2} ->
            grab(T2, R, ["</blockquote>",
                         make_escape_string(R1, R),
                         "<blockquote>" | Acc], W);
        {{esc_false, R1}, _T2} ->
            {R1, lists:reverse(Acc), false};
        {false, T2} ->
            case is_double_indent(S) of
                false ->
                    {List, lists:reverse(Acc), false};
                {true, R2} ->
                    %% if it is a double indent - delete 4 spaces
                    %% no it makes not sense to me neither :(
                    grab(T2, R, ["    " ++ make_escape_string(R2, R) | Acc], W)
            end
    end;
grab([{linefeed, _} | T], R, Acc, false) ->
    grab2(T, R, Acc, T, Acc, true);
grab([{linefeed, _} | T], R, Acc, true) ->
    grab2(T, R, ["\n" | Acc], T, Acc, true);
grab([{blank, _} | T], R, Acc, false) ->
    grab2(T, R, Acc, T, Acc, true);
grab([{blank, _} | T], R, Acc, true) ->
    grab2(T, R, ["\n" | Acc], T, Acc, true);
grab([{normal, P} | T], R, Acc, W) ->
    Li = case W of
             false ->
                 make_escape_string(P, R);
             true  ->
                 "<p>"++ string:strip(make_escape_string(P, R), right, ?LF)
                     ++ "</p>"
         end,
    grab(T, R, [Li | Acc], W);
grab(List, _R, Acc, W) ->
    {List, lists:reverse(Acc), W}.

%% the problem is knowing when to grab, if the list is followed by a long
%% string of blank lines and linefeeds and a normal then the linefeeds aren't
%% grabbed
%% if the list if followed by blank lines and linefeeds and a normal with an
%% initial whitespace it is grabbed...
grab2([{normal, P2} | T], R, Acc, LO, AO, W) ->
    case P2 of
        [{{ws, _}, _} | T2] ->
            Li = case W of
                     false ->
                         make_escape_string(T2, R);
                     true  ->
                         "<p>" ++
                             string:strip(make_escape_string(T2, R), right, ?LF)
                             ++ "</p>"
                 end,
            grab(T, R, [Li | Acc], W);
        _ ->
            {LO, AO, false}
    end;
grab2([{linefeed, _} | T], R, Acc, LO, AO, _W) ->
    grab2(T, R, ["\n" | Acc], LO, AO, true);
grab2([{blank, _} | T], R, Acc, LO, AO, _W) ->
    grab2(T, R, ["\n" | Acc], LO, AO, true);
%% We dont want to grab this stuff so return the old list and the old acc
grab2(_List, _R, _Acc, LO, AO, _W) ->
    {LO, AO, true}.

is_double_indent(List) ->
    is_double_indent1(List, 0).

%% double indent is any combination of tabs and spaces that add
%% up to 8
is_double_indent1([], _N) ->
    false;
is_double_indent1(Rest, N)
  when N > 7 ->
    {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N)  ->
    is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) ->
    is_double_indent1(T, N + 4);
is_double_indent1(_List, _N) ->
    false.

is_blockquote(List, T) ->
    case is_bq1(List, 0) of
        false ->
            {false, T};
        {esc_false, R} ->
            {{esc_false, R}, T};
        {true, R} ->
            {NewT, NewR} = grab2(T, R),
            {{true, NewR}, NewT}
    end.

is_bq1([], _N) ->
    false;
is_bq1([{{ws, sp}, _} | T], N) ->
    is_bq1(T, N + 1);
is_bq1([{{ws, tab}, _} | T], N) ->
    is_bq1(T, N + 4);
is_bq1([{{md, gt}, _}, {{ws, _}, _} | T], N)
  when N > 3  ->
    {true, T};
is_bq1([{{punc, bslash}, _}, {{md, gt}, GT}, {{ws, _}, WS} | T], N)
  when N > 3 ->
    {esc_false, [GT, WS | T]};
is_bq1(_List, _N) ->
    false.

grab2(List, R) ->
    gb2(List, lists:reverse(R)).

gb2([], Acc) ->
    {[], lists:flatten(lists:reverse(Acc))};
gb2([{blank, _} | T], Acc) ->
    {T, lists:flatten(lists:reverse(Acc))};
gb2([{_Type, P} | T], Acc) ->
    gb2(T, [P | Acc]).

%%
%% Make the lines from the raw tokens
%%
make_lines(Tokens) ->
    ml1(Tokens, [], []).

ml1([], [], A2) ->
    lists:reverse(A2);
ml1([], A1, A2) ->
    ml1([], [], [lists:reverse(A1) | A2]);
ml1([{{lf, _}, _} = H | T], A1, A2) ->
    ml1(T, [], [ml2(H, A1) | A2]);
ml1([H | T], A1, A2) ->
    ml1(T, [H | A1], A2).

ml2(H, List) ->
    lists:reverse([H | List]).

%%%
%%% Process the lines and give each line a type. The valid types are:
%%% * normal line
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - unordered lists (including code blocks)
%%%   - ordered lists (including code blocks)
%%%   - blockquotes
%%%   - code blocks
%%%   - horizontal rules
%%%
type_lines(Lines) ->
    {Refs, TypedLines} = type_lines1(Lines, [], []),
    {strip_lines(TypedLines), Refs}.

type_lines1([], A1, A2) ->
    {A1, lists:reverse(A2)};
type_lines1([[{{ws, sp}, _}, {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    %% this clause extracts URL and Image refs
    %% (it is the only one that uses A1 and A2...
    %% inlines can have up to 3 spaces before it
    t_inline(H, T1, T2, A1, A2);
type_lines1([[{{ws, tab}, _}, {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
type_lines1([[{{ws, comp}, W}, {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    case gt(W, 3) of
        {true, _R} ->
            t_inline(H, T1, T2, A1, A2);
        false      ->
            type_lines1(T1, A1, [{normal , H} | A2]) % same exit at the final clause!
    end,
    t_inline(H, T1, T2, A1, A2);
type_lines1([[{{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
type_lines1([[{{md, eq}, _} | _T] = H | T], A1, A2) ->
    %% types setext lines
    type_lines1(T, A1, [type_setext_h1(H) | A2]);
type_lines1([[{{md, dash}, _} | _T] = H | T], A1, A2) ->
    %% NOTE 1: generates a ul as the default not a normal line
    %% NOTE 2: depending on the context this might generate an <h2> header
    %%         or an <hr />
    %% NOTE 3: space - is typed to a bullet down in <ul> land...
    type_lines1(T, A1, [type_setext_h2(H) | A2]);
type_lines1([[{{md, atx}, _} | _T] = H | T], A1, A2) ->
    %% types atx lines
    type_lines1(T, A1, [type_atx(H) | A2]);
type_lines1([[{{md, gt}, _} | []] = H | T], A1, A2) ->
    %% types blockquotes
    %% a blockquote on its own or followed by a linefeed is
    %% displayed 'as is' by showdown
    type_lines1(T, A1, [{normal, H} | A2]);
type_lines1([[{{md, gt}, _}, {{lf, _}, _} | []] = H | T], A1, A2) ->
    type_lines1(T, A1, [{normal, H} | A2]);
%% one with anything after it starts a blockquote
type_lines1([[{{md, gt}, _} | _T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [{blockquote, H} | A2]);
type_lines1([[{{ws, _}, _}, {{md, star}, _} = ST1, {{ws, _}, _} = WS1 | T1] = H | T], A1, A2) ->
    %% types unordered lists lines
    %% NOTE 1: the dashed version is generated in type_setext_h2
    %% NOTE 2: the asterix version also might generate a horizontal rule
    %%         which is why it jumps to type_star2 <-- note the 2!!
    type_lines1(T, A1, [{type_star2([ST1, WS1 | T1]), H} | A2]);
type_lines1([[{{md, star}, _}, {{ws, _}, _} | _T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [{type_star2(H), H} | A2]);
type_lines1([[{{ws, _}, _}, {{md, plus}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);
type_lines1([[{{md, plus}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);
%% UL based on dashes
type_lines1([[{{ws, _}, _}, {{md, dash}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);
type_lines1([[{{ws, _}, _}, {num, _} = N1| T1] | T], A1, A2) ->
    %% types ordered lists...
    type_lines1(T, A1, [type_ol([N1 | T1]) | A2]);
type_lines1([[{num, _} | _T] = H | T], A1, A2) ->
    type_lines1(T, A1, [type_ol(H) | A2]);
type_lines1([[{{md, underscore}, _} | _T1] = H | T], A1, A2) ->
    %% types horizontal rules for stars and underscores
    %% dashes and some stars are done elsewhere...
    type_lines1(T, A1, [type_underscore(H) | A2]);
type_lines1([[{{md, star}, _} | _T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [type_star(H) | A2]);
type_lines1([[{{{tag, _Type}, Tag}, _ } = H | T1] = List | T], A1, A2) ->
    %% Block level tags - these are look ahead they must be
    %% on a single line (ie directly followed by a lf and nothing else
    case is_blank(T1) of
        false ->
            type_lines1(T, A1, [{normal , List} | A2]);
        true  ->
            case is_block_tag(Tag) of
                true  ->
                    type_lines1(T, A1, [{blocktag , [H]} | A2]);
                false ->
                    type_lines1(T, A1, [{tag, [H | T1]} | A2])
            end
    end;
type_lines1([[{{lf, _}, _}| []]  = H | T], A1, A2) ->
    %% types a blank line or a code block
    type_lines1(T, A1, [{linefeed, H} | A2]);
type_lines1([[{{ws, _}, _} | _T1] = H | T], A1, A2) ->
    type_lines1(T, A1, [type_ws(H) | A2]);

%% Final clause...
type_lines1([H | T], A1, A2) ->
    type_lines1(T, A1, [{normal , H} | A2]).

t_inline(H, T1, T2, A1, A2) ->
    case snip_ref(T1) of
        {Type, {Id, {Url, Title}}} ->
            type_lines1(T2, lists:flatten([{Id, {Url, Title}} | A1]),
                        [{Type, H} | A2]);
        normal ->
            type_lines1(T2, A1, [{normal, H} | A2])
    end.

%% strips blanks from the beginning and end
strip_lines(List) ->
    lists:reverse(strip_lines1(lists:reverse(strip_lines1(List)))).

strip_lines1([{linefeed, _} | T]) ->
    strip_lines1(T);
strip_lines1([{blank, _} | T]) ->
    strip_lines1(T);
strip_lines1(List)  ->
    List.

%%
%% Loads of type rules...
%%
is_blank([]) ->
    true;
is_blank([{{lf, _}, _} | []]) ->
    true;
is_blank([{{ws, _}, _} | T])  ->
    is_blank(T);
is_blank(_List) ->
    false.

is_block_tag("address") ->
    true;
is_block_tag("blockquote") ->
    true;
is_block_tag("center") ->
    true;
is_block_tag("dir") ->
    true;
is_block_tag("div") ->
    true;
is_block_tag("dl") ->
    true;
is_block_tag("fieldset") ->
    true;
is_block_tag("form") ->
    true;
is_block_tag("h1") ->
    true;
is_block_tag("h2") ->
    true;
is_block_tag("h3") ->
    true;
is_block_tag("h4") ->
    true;
is_block_tag("h5") ->
    true;
is_block_tag("h6") ->
    true;
is_block_tag("hr") ->
    true;
is_block_tag("isindex") ->
    true;
is_block_tag("menu") ->
    true;
is_block_tag("noframes") ->
    true;
is_block_tag("noscript") ->
    true;
is_block_tag("ol") ->
    true;
is_block_tag("p") ->
    true;
is_block_tag("pre") ->
    true;
is_block_tag("table") ->
    true;
is_block_tag("thead") ->
    true;
is_block_tag("tbody") ->
    true;
is_block_tag("tr") ->
    true;
is_block_tag("td") ->
    true;
is_block_tag("ul") ->
    true;
is_block_tag(_Other) ->
    false.

type_underscore(List) ->
    case type_underscore1(trim_right(List)) of
        hr ->
            {hr, List};
        maybe ->
            {type_underscore2(List), List}
    end.

type_underscore1([]) ->
    hr;
type_underscore1([{{md, underscore}, _} | T]) ->
    type_underscore1(T);
type_underscore1(_List) ->
    maybe.

type_underscore2(List) ->
    case trim_right(List) of % be permissive of trailing spaces
        [{{md, underscore}, _}, {{ws, _}, _},
         {{md, underscore}, _}, {{ws, _}, _},
         {{md, underscore}, _}] ->
            hr;
        _Other ->
            normal
    end.

type_star(List) ->
    Trim = trim_right(List),
    case type_star1(Trim) of % be permssive of trailing spaces
        hr ->
            {hr, trim_right(Trim)};
        maybe ->
            Type = type_star2(List),
            %% if it is a normal line we prepend it with a special
            %% non-space filling white space character
            case Type of
                normal ->
                    {normal, [{{ws, none}, none} | List]};
                _ ->
                    {Type, List}
            end
    end.

type_star1([]) ->
    hr;
type_star1([{{md, star}, _} | T]) ->
    type_star1(T);
type_star1(_List) ->
    maybe.

type_star2(List) ->
    case trim_right(List) of
        [{{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}] ->
            hr;
        _Other ->
            case List of
                [{{md, star}, _},
                 {{ws, _}, _}= WS | T] ->
                    {ul, make_list_str([WS | T])};
                _Other2  ->
                    normal
            end
    end.

type_ol(List) ->
    case type_ol1(List, []) of
        normal ->
            {normal, List};
        {ol, Str} ->
            {{ol, Str}, List};
        {esc_normal, Str} ->
            {normal, Str}
    end.

%% this line terminates on an escaped fullstop after a number
%% (but you need to drop the bslash...)
type_ol1([{num, _} = N,
          {{punc, bslash}, _},
          {{punc, fullstop}, _} = P | T], Acc) ->
    {esc_normal, lists:flatten([lists:reverse(Acc), N, P | T])};
type_ol1([{num, _} = H | T], Acc)  ->
    %% we accumulate the digits in case we need to escape a full stop in a normal line
    type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _}, {{ws, _}, _} | T], _Acc) ->
    {ol, T};
type_ol1(_List, _Acc) ->
    normal.

%% You need to understand what this function is trying to d...
%% '### blah' is fine
%% '### blah ###' is reduced to '### blah' because trailing #'s are
%% just for show but...
%% '##' is like appling '#' to '#' <-- applying 1 less styling to a single #
%% and '###' is like appling '##' to '#' etc, etc
%% but after you hit 6#'s you just get this for a single hash
%% ie '#############' is like applying '######' to a single '#'
%% but/and '######## blah' is like apply '######' to '## blah'
%% strip trailing #'s as they are decorative only...
type_atx(List) ->
    {Sz, R} = get_atx_size(List),
    A = [{{md, atx}, "#"}],
    Type =
        case is_all_hashes(R) of
            true  ->
                if
                    Sz == 1 ->
                        normal;
                    ((Sz > 1) andalso (Sz < 6)) ->
                        Ns = integer_to_list(Sz - 1),
                        Hn = list_to_atom("h" ++ Ns),
                        {Hn, A};
                    ((Sz == 6) andalso (R == [])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, lf}, "\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, crlf}, "\r\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R =/= [])) ->
                        {h6, A}
                end;
            false ->
                Ns = integer_to_list(Sz),
                Hn = list_to_atom("h" ++ Ns),
                {Hn, strip_atx(R)}
        end,
    {Type, List}.

is_all_hashes([]) ->
    true;
is_all_hashes([{{md, atx}, _} | T]) ->
    is_all_hashes(T);
is_all_hashes([{{lf, _}, _} | []])  ->
    true;
is_all_hashes(_List) ->
    false.

get_atx_size(List) ->
    g_atx_size1(List, 0).

                                                % this function also strips whitespace to the left...
g_atx_size1([{{md, atx}, _} = A | T], N) when N == 6 ->
    {6, [A | T]};
g_atx_size1([{{md, atx}, _} | T], N) ->
    g_atx_size1(T, N + 1);
g_atx_size1([{{ws, _}, _} | T], N) ->
    g_atx_size1(T, N);
g_atx_size1(List, N) ->
    {N, List}.

strip_atx(List) ->
    lists:reverse(s_atx1(lists:reverse(List))).

s_atx1([{{lf, _}, _}, {{md, atx}, _} | T]) ->
    s_atx1(T);
s_atx1([{{md, atx}, _} | T])  ->
    s_atx1(T);
s_atx1(List) ->
    List.

type_setext_h1(List) ->
    type_s_h1_1(List, []).

%% terminates on running out or new line
type_s_h1_1([{{lf, _}, _} = L | []], Acc) ->
    {setext_h1, lists:reverse([L | Acc])};
type_s_h1_1([], Acc)->
    {setext_h1, lists:reverse(Acc)};
type_s_h1_1([[] | T], Acc) ->
    type_s_h1_1(T, Acc);
type_s_h1_1([{{md, eq}, _} = H | T], Acc) ->
    type_s_h1_1(T, [H | Acc]);
type_s_h1_1(L, Acc) ->
    {normal, lists:flatten([Acc | L])}.

type_setext_h2(List) ->
    case type_s_h2_1(List) of
        h2_or_hr ->
            {h2_or_hr, List};
        not_h2 ->
            {type_s_h2_2(trim_right(List)), List}
    end.
%% terminates on running out or new line
type_s_h2_1([{{lf, _}, _} | []])   ->
    h2_or_hr;
type_s_h2_1([]) ->
    h2_or_hr;
type_s_h2_1([[] | T]) ->
    type_s_h2_1(T);
type_s_h2_1([{{md, dash}, _} | T]) ->
    type_s_h2_1(T);
type_s_h2_1(_L) ->
    not_h2.

type_s_h2_2([{{md, dash}, _}, {{ws,_}, _},
             {{md, dash}, _}, {{ws, _}, _},
             {{md, dash}, _}]) ->
    hr;
type_s_h2_2([{{md, dash}, _},
             {{ws, _}, _} = WS | T]) ->
    {ul, make_list_str([WS | T])};
type_s_h2_2(_List) ->
    normal.

type_ws(List) ->
    case type_ws1(List) of
        blank ->
            {blank, List};
        try_codeblock ->
            case type_ws2(List) of
                normal ->
                    {normal, List};
                {codeblock, Ret} ->
                    {{codeblock, Ret}, List}
            end
    end.

type_ws1([]) ->
    blank;
type_ws1([{{lf, _}, _} | []]) ->
    blank;
type_ws1([[] | T]) ->
    type_ws1(T);
type_ws1([{{ws, _}, _} | T])  ->
    type_ws1(T);
type_ws1(_L) ->
    try_codeblock.

%% 4 or more spaces takes you over the limit
%% (a tab is 4...)
type_ws2([{{ws, tab}, _} | T])  ->
    {codeblock, T};
type_ws2([{{ws, comp}, W} | T]) ->
    case gt(W, 4) of
        {true, R} ->
            {codeblock, [R| T]};
        false ->
            normal
    end;
type_ws2([{{ws, sp}, _} | _T])  ->
    normal.

gt(String, Len) ->
    ExpString = re:replace(String, "\t", "    ", [{return, list}]),
    ExpStringLen = length(ExpString),
    if
        ExpStringLen >= Len ->
            WS = string:substr(ExpString, Len + 1,
                               ExpStringLen),
            {true, {{ws, sp}, WS}};
        ExpStringLen <  Len ->
            false
    end.

%% make a tag into a string
make_tag_str(L, R) ->
    make_tag1(L, R, []).

make_tag1([], _R, Acc) ->
    lists:reverse(Acc);
make_tag1([{{{tag, _Type}, _Tag}, B} | T], R, Acc) ->
    make_tag1(T, R, [B | Acc]);
make_tag1([H | T], R, Acc) ->
    make_tag1(T, R, [make_string([H], R) | Acc]).

esc_tag(String) ->
    esc_tag1(String, []).

esc_tag1([], Acc) ->
    lists:reverse(Acc);
esc_tag1([?NBSP | T], Acc) ->
    esc_tag1(T, [?SPACE | Acc]); % non-breaking space to space
esc_tag1([H | T], Acc) ->
    esc_tag1(T, [H | Acc]).

%% if it is a list we need to discard the initial white space...
make_list_str([{{ws, _}, _} | T] = List) ->
    case is_double_indent(List) of
        false ->
            T;
        {true, R} ->
            lists:flatten([{tags, "<pre><code>"} ,R ,
                           {tags, "</code></pre>\n\n"} | []])
    end.

%% All ref processing can ignore the original values 'cos those
%% have already been captured at a higher level
snip_ref(List) ->
    case get_id(List) of
        {[{_, Id}], Rest} ->
            {_Rest2, Ref, Title} = parse_inline(Rest),
            Ref2 = trim(Ref),
            Rs = htmlencode(make_plain_string(Ref2)),
            Ts = make_plain_string(Title),
            {inlineref, {Id, {Rs, Ts}}};
        normal ->
            normal
    end.

get_id(List) ->
    g_id1(List, []).

g_id1([], _Acc) ->
    normal;
g_id1([{{inline, close}, _}, {{punc, colon}, _}, {{ws, _}, _} | T], Acc) ->
    {lists:reverse(Acc), T};
g_id1([H | T], Acc) ->
    g_id1(T, [H | Acc]).

parse_inline(List) ->
    parse_inline1(List, []).

%% snip off the terminal linefeed (if there is one...)
parse_inline1([{{lf, _}, _} | []], A) ->
    {[], lists:reverse(A), []};
parse_inline1([], A) ->
    {[], lists:reverse(A), []};
parse_inline1([{{punc, bslash}, _}, {bra, _} = B | T], A) ->
    %% brackets can be escaped
    parse_inline1(T, [B | A]);
parse_inline1([{{punc, bslash}, _}, {ket, _} = B | T], A) ->
    parse_inline1(T, [B | A]);
parse_inline1([{{punc, bslash}, _}, {{punc, doubleq}, _} = Q | T], A) ->
    parse_inline1(T, [Q | A]);
parse_inline1([{{punc, bslash}, _}, {{punc, singleq}, _} = Q | T], A) ->
    parse_inline1(T, [Q | A]);
parse_inline1([{{punc, doubleq}, _} | T], A) ->
    %% these clauses capture the start of the title...
    parse_inline2(T, lists:reverse(A), doubleq, []);
parse_inline1([{{punc, singleq}, _} | T], A) ->
    parse_inline2(T, lists:reverse(A), singleq, []);
parse_inline1([{bra, _} | T], A) ->
    parse_inline2(T, lists:reverse(A), brackets, []);
parse_inline1([{ket, _} | T], A) ->
    {T, lists:reverse(A), []};
parse_inline1([H | T], A) ->
    parse_inline1(T, [H | A]).

%% this gets titles in single and double quotes
%% the delimiter type is passed in as 'D'
parse_inline2([], Url, _D, A) ->
    {[], Url, lists:flatten(lists:reverse(A))};

parse_inline2([{{punc, bslash}, _}, {bra, _} = B | T], Url, D, A) ->
    %% brackets can be escaped
    parse_inline2(T, Url, D, [B | A]);
parse_inline2([{{punc, bslash}, _}, {ket, _} = B | T], Url, D, A) ->
    parse_inline2(T, Url, D, [B | A]);
parse_inline2([{{punc, bslash}, _},
               {{punc, doubleq}, _}= Q | T], Url, D, A)    ->
    %% quotes can be escaped
    parse_inline2(T, Url, D, [Q | A]);
parse_inline2([{{punc, bslash}, _}, {{punc, singleq}, _} = Q | T], Url, D, A) ->
    parse_inline2(T, Url, D, [Q | A]);
parse_inline2([{{punc, doubleq}, _} | T], Url, doubleq, A) ->
    %% these clauses capture the end of the title and drop the delimiter...
    parse_inline2(T, Url, none, A);
parse_inline2([{{punc, singleq}, _} | T], Url, singleq, A) ->
    parse_inline2(T, Url, none, A);
parse_inline2([{ket, _} | T], Url, brackets, A) ->
    parse_inline2(T, Url, none, A);
parse_inline2([{ket, _} | T], Url, none, A) ->
    %% terminator clause
    {T, Url, lists:flatten(lists:reverse(A))};
parse_inline2([_H | T], Url, none, A) ->
    %% this clause silently discards stuff after the delimiter...
    parse_inline2(T, Url, none, [A]);
parse_inline2([H | T], Url, D, A) ->
    parse_inline2(T, Url, D, [H | A]).

trim(String) ->
    trim_left(trim_right(String)).

trim_right(String) ->
    lists:reverse(trim_left(lists:reverse(String))).

trim_left([{{ws, _}, _} | T]) ->
    trim_left(T);
trim_left([[] | T]) ->
    trim_left(T);
trim_left(List) ->
    List.

snip(List) ->
    List2 = lists:reverse(List),
    case List2 of
        [{{lf, _}, _} | T] ->
            lists:reverse(T);
        _  ->
            List
    end.

%% end of ref processing

%%
%% Build the Lexed Token List
%% This is a two part lexer, first it chunks the input and then on the second
%% pass it gathers it into lines and types the lines
%%
%% NOTE that there are two different styles of processing lines:
%% * markdown transformed
%% * block
%% inside block processing the whole text is dumped and just url encoded
%% and the original text is always maintained during the lexing/parsing
%% so that it can be recreated if the context requires it...
%%

lex(String) ->
    merge_ws(lex1(String, [], [])).

merge_ws(List) ->
    merge_ws1(List, []).

merge_ws1([], Acc) ->
    lists:reverse(Acc);
merge_ws1([{{ws, _}, W1}, {{ws, _}, W2} | T], Acc) ->
    merge_ws1([{{ws, comp}, W1 ++ W2} | T], Acc);
merge_ws1([H | T], Acc) ->
    merge_ws1(T, [H | Acc]).

%% this is the terminal head which ends the parsing...
lex1([], [], A2) ->
    lists:flatten(lists:reverse(A2));
lex1([], A1, A2) ->
    lex1([], [], [lex2(A1) | A2]);
%% these two heads capture opening and closing tags
lex1([$<, $/|T], A1, A2) ->
    {Tag, NewT} = closingdiv(T, []),
    lex1(NewT, [], [Tag, lex2(A1) | A2]);
lex1([$< | T], A1, A2) ->
    {Tag, NewT} = openingdiv(T),
    lex1(NewT, [], [Tag , lex2(A1) | A2]);
%% these clauses are the normal lexer clauses
lex1([$= | T], A1, A2) ->
    lex1(T, [], [{{md, eq}, "="},  lex2(A1) | A2]);
lex1([$- | T], A1, A2) ->
    lex1(T, [], [{{md, dash}, "-"}, lex2(A1) | A2]);
lex1([$# | T], A1, A2) ->
    lex1(T, [], [{{md, atx}, "#"},  lex2(A1) | A2]);
lex1([$> | T], A1, A2) ->
    lex1(T, [], [{{md, gt}, ">"},   lex2(A1) | A2]);
lex1([$+ | T], A1, A2) ->
    lex1(T, [], [{{md, plus}, "+"}, lex2(A1) | A2]);
lex1([$* | T], A1, A2) ->
    lex1(T, [], [{{md, star}, "*"}, lex2(A1) | A2]);
lex1([$_ | T], A1, A2) ->
    lex1(T, [], [{{md, underscore}, "_"}, lex2(A1) | A2]);
lex1([$1 | T], A1, A2) ->
    lex1(T, [], [{num, "1"}, lex2(A1) | A2]);
lex1([$2 | T], A1, A2) ->
    lex1(T, [], [{num, "2"}, lex2(A1) | A2]);
lex1([$3 | T], A1, A2) ->
    lex1(T, [], [{num, "3"}, lex2(A1) | A2]);
lex1([$4 | T], A1, A2) ->
    lex1(T, [], [{num, "4"}, lex2(A1) | A2]);
lex1([$5 | T], A1, A2) ->
    lex1(T, [], [{num, "5"}, lex2(A1) | A2]);
lex1([$6 | T], A1, A2) ->
    lex1(T, [], [{num, "6"}, lex2(A1) | A2]);
lex1([$7 | T], A1, A2) ->
    lex1(T, [], [{num, "7"}, lex2(A1) | A2]);
lex1([$8 | T], A1, A2) ->
    lex1(T, [], [{num, "8"}, lex2(A1) | A2]);
lex1([$9 | T], A1, A2) ->
    lex1(T, [], [{num, "9"}, lex2(A1) | A2]);
lex1([$0 | T], A1, A2) ->
    lex1(T, [], [{num, "0"}, lex2(A1) | A2]);
lex1([$. | T], A1, A2) ->
    lex1(T, [], [{{punc, fullstop}, "."}, lex2(A1) | A2]);
lex1([$: | T], A1, A2) ->
    lex1(T, [], [{{punc, colon}, ":"}, lex2(A1) | A2]);
lex1([$' | T], A1, A2) ->
    lex1(T, [], [{{punc, singleq}, "'"}, lex2(A1) | A2]); %'
lex1([$" | T], A1, A2) ->
    lex1(T, [], [{{punc, doubleq}, "\""}, lex2(A1) | A2]); %"
lex1([$` | T], A1, A2) ->
    lex1(T, [], [{{punc, backtick}, "`"}, lex2(A1) | A2]); %"
lex1([$! | T], A1, A2) ->
    lex1(T, [], [{{punc, bang}, "!"}, lex2(A1) | A2]); %"
lex1([$\\ | T], A1, A2)->
    lex1(T, [], [{{punc, bslash}, "\\"}, lex2(A1) | A2]); %"
lex1([$/ | T], A1, A2) ->
    lex1(T, [], [{{punc, fslash}, "/"}, lex2(A1) | A2]); %"
lex1([$( | T], A1, A2) ->
    lex1(T, [], [{bra, "("}, lex2(A1) | A2]);
lex1([$) | T], A1, A2) ->
    lex1(T, [], [{ket, ")"}, lex2(A1) | A2]);
lex1([$[ | T], A1, A2) ->
    lex1(T, [], [{{inline, open}, "["}, lex2(A1) | A2]);
lex1([$] | T], A1, A2) ->
    lex1(T, [], [{{inline, close}, "]"}, lex2(A1) | A2]);
lex1([?SPACE | T], A1, A2) ->
    %% note there is a special 'whitespace' {{ws, none}, ""} which is used to generate non-space
    %% filling whitespace for cases like '*bob* is great' which needs a non-space filling
    %% whitespace prepended to trigger emphasis so it renders as "<em>bob</em> is great...
    %% that 'character' doesn't exist so isn't in the lexer but appears in the parser
    lex1(T, [], [{{ws, sp}, " "}, lex2(A1) | A2]);
lex1([?TAB | T], A1, A2) ->
    lex1(T, [], [{{ws, tab}, "\t"}, lex2(A1) | A2]);
lex1([?NBSP | T], A1, A2) ->
    lex1(T, [], [{{ws, sp}, "&nbsp"}, lex2(A1) | A2]);
lex1([?CR, ?LF | T], A1, A2) ->
    lex1(T, [], [{{lf, crlf}, [?CR , ?LF]}, lex2(A1) | A2]);
lex1([?LF | T], A1, A2) ->
    lex1(T, [], [{{lf, lf}, [?LF]}, lex2(A1) | A2]);
lex1([H|T], A1, A2) ->
    %% this final clause accumulates line fragments
    lex1(T, [H |A1] , A2).

lex2([]) ->
    [];
lex2(List) ->
    {string, lists:flatten(lists:reverse(List))}.

%% need to put in regexes for urls and e-mail addies
openingdiv(String) ->
    case get_url(String) of
        {{url, URL}, R1} ->
            {{url, URL}, R1};
        not_url ->
            case get_email_addie(String) of
                {{email, EM}, R2} ->
                    {{email, EM}, R2};
                not_email ->
                    openingdiv1(String, [])
            end
    end.

                                                % dumps out a list if it is not an opening div
openingdiv1([], Acc) ->
    {lists:flatten([{{punc, bra}, "<"} | lex(lists:reverse(Acc))]), []};
openingdiv1([$/,$>| T], Acc) ->
    Acc2 = lists:flatten(lists:reverse(Acc)),
    Acc3 = string:to_lower(Acc2),
    [Tag | _T] = string:tokens(Acc3, " "),
    {{{{tag, self_closing}, Tag}, "<"
      ++ Acc2 ++ "/>"}, T};
openingdiv1([$>| T], []) ->
    %% special for non-tags
    {[{{punc, bra}, "<"},
      {{punc, ket}, ">"}], T};
openingdiv1([$>| T], Acc) ->
    Acc2 = lists:flatten(lists:reverse(Acc)),
    Acc3 = string:to_lower(Acc2),
    [Tag | _T] = string:tokens(Acc3, " "),
    {{{{tag, open}, Tag}, "<"
      ++ Acc2 ++ ">"}, T};
openingdiv1([H|T], Acc) ->
    openingdiv1(T, [H | Acc]).

                                                % dumps out a list if it is not an closing div
closingdiv([], Acc) ->
    {lists:flatten([{{punc, bra}, "<"}, {{punc, fslash}, "/"}
                    | lex(lists:reverse(Acc))]), []};
closingdiv([$>| T], Acc) ->
    Acc2 = lists:flatten(lists:reverse(Acc)),
    Acc3 = string:to_lower(Acc2),
    [Tag | _T] = string:tokens(Acc3, " "),
    {{{{tag, close}, Tag}, "</"
      ++ Acc2 ++ ">"}, T};
closingdiv([H|T], Acc) ->
    closingdiv(T, [H | Acc]).

get_url(String) ->
    HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
    case re:run(String, HTTP_regex) of
        nomatch ->
            not_url;
        {match, _} ->
            get_url1(String, [])
    end.

get_url1([], Acc) ->
    URL = lists:flatten(lists:reverse(Acc)),
    {{url, URL}, []};
get_url1([$\\, $> | T], Acc) ->
    %% allow escaped kets
    get_url1(T, [$>, $\\ | Acc]);
get_url1([$> | T], Acc) ->
    URL = lists:flatten(lists:reverse(Acc)),
    {{url, URL}, T};
get_url1([H | T], Acc) ->
    get_url1(T, [H | Acc]).

get_email_addie(String) ->
    Snip_regex = ">",
    case re:run(String, Snip_regex) of
        nomatch ->
            not_email;
        {match, [{N, _} | _T]} ->
            {Possible, [$> | T]} = lists:split(N, String),
            EMail_regex = "[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+"
                ++ "(?:\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                ++ "@(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\.)+"
                ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
                ++ "|biz|info|mobi|name|aero|jobs|museum)",
            case re:run(Possible, EMail_regex) of
                nomatch ->
                    not_email;
                {match, _} ->
                    {{email, Possible}, T}
            end
    end.

make_plain_string(List) ->
    make_plain_string(List, []).

make_plain_string([], Acc) ->
    lists:flatten(lists:reverse(Acc));
make_plain_string([{{ws, none}, none} | T], Acc) ->
    make_plain_string(T, [" " | Acc]);
make_plain_string([{_, Str} | T], Acc) ->
    make_plain_string(T, [Str | Acc]).

make_escape_string(List, Refs) ->
    make_escape_string(List, Refs, []).

make_escape_string([], _R, A)  ->
    lists:flatten(lists:reverse(A));
make_escape_string([{tags, Tag} | T], R, A) ->
    make_escape_string(T, R, [{tags, Tag} | A]);
make_escape_string([H | T], R, A)  ->
    make_escape_string(T, R, [make_string([H], R) | A]).


make_string(List, Refs) ->
    make_string(List, Refs, []).

make_string([], _R, A) ->
    Flat = lists:flatten(lists:reverse(A)),
    htmlchars(Flat);
make_string([{{punc, bang}, B}, {{inline, open}, O} | T], R, A) ->
    case get_inline(T, R, [], img) of
        {Rest, {Url, Title, Acc}} ->
            Tag = [make_img_tag(Url, Acc, Title)],
            make_string(Rest, R, [Tag | A]);
        {Rest, Tag} ->
            make_string(Rest, R, [Tag, O, B | A])
    end;
%% escape inline open's...
make_string([{{punc, bslash}, _}, {{inline, open}, O} | T], R, A) ->
    make_string(T, R, [O | A]);
make_string([{{inline, open}, O} | T], R, A) ->
    case get_inline(T, R, [], url) of
        {Rest, {Url, Title, Acc}} ->
            Tit = case Title of
                      [] ->
                          [];
                      _  ->
                          " title=\"" ++ Title ++ "\""
                  end,
            Tag = [{tags, "<a href=\"" ++ Url ++ "\""
                    ++ Tit ++ ">"}, Acc,
                   {tags, "</a>"} | []],
            make_string(Rest, R, [Tag | A]);
        {Rest, Tag} ->
            make_string(Rest, R, [Tag, O | A])
    end;
make_string([{email, Addie} | T], R, A) ->
    make_string(T, R, [{tags, "\" />"}, Addie, {tags, "<a href=\"mailto:"}| A]);
make_string([{url, Url} | T], R, A) ->
    make_string(T, R, [ {tags, "</a>"}, Url, {tags, "\">"}, Url,
                        {tags, "<a href=\""} | A]);
make_string([{tags, _} = Tag | T], R, A) ->
    make_string(T, R, [Tag | A]);
make_string([{{{tag, Type}, Tag}, _} | T], R, A) ->
    Tag2 = esc_tag(Tag),
    TagStr = case Type of
                 open ->
                     {tags, "&lt;"  ++ Tag2 ++ "&gt;"};
                 close ->
                     {tags, "&lt;/" ++ Tag2 ++ "&gt;"};
                 self_closing ->
                     {tags, "&lt;"  ++ Tag2 ++ " /&gt;"}
             end,
    make_string(T, R, [TagStr | A]);
make_string([{_, Orig} | T], R, A)  ->
    make_string(T, R, [Orig | A]).

get_inline([], _R, A, _) ->
    %% if the inline doesn't terminate its not an inline...
    {[], make_plain_string(lists:reverse(A))};
get_inline([{{punc, bang}, _B}, {{inline, open}, _O} | T], R, A, url) ->
    %% a url can contain an image inline
    {Rest, {Url, Title, Acc}} = get_inline(T, R, A, img),
    Tag = make_img_tag(Url, Acc, Title),
    %% We double tag the tag so that it can get through the lists:flatteners..
    get_inline(Rest, R, [{tags, Tag} | A], url);
get_inline([{{inline, close}, _}, {bra, _} | T], _R, A, _) ->
    {Rest, Url, Title} = parse_inline(T),
    Tag = {string:strip(make_plain_string(Url)),
           make_plain_string(Title),
           make_plain_string(lists:reverse(A))},
    {Rest, Tag};
get_inline([{{inline, close}, _}, {{ws, sp}, _}, {bra, _} | T], _R, A, img) ->
    %% for img's but not url's you need to allow a single space between them
    %% to be compatible with showdown :(
    {Rest, Url, Title} = parse_inline(T),
    Tag = {string:strip(make_plain_string(Url)),
           make_plain_string(Title),
           make_plain_string(lists:reverse(A))},
    {Rest, Tag};
get_inline([{{inline, close}, _}, {{inline, open}, _} | T], R, A, _) ->
    %% this clause detects references to images/links...
    Text = make_plain_string(lists:reverse(A)),
    case get_id_diff(T) of
        normal ->
            {[], make_plain_string(lists:reverse(A))};
        {[{_, Id}], Rest} ->
            {Url, Title} = case lists:keyfind(Id, 1, R) of
                               false ->
                                   {"", ""};
                               {Id, {U, Tit}} ->
                                   {U, Tit}
                           end,
            Tag = {Url, Title, Text},
            {Rest, Tag};
        _Other ->
            {[], make_plain_string(lists:reverse(A))} % random failing id's
    end;
get_inline([{{inline, close}, _} = C , {{ws, _}, _},
            {{inline, open}, _} = O | T], R, A, Type) ->
    %% so does this one - just delete the space and rethrow it
    get_inline([C, O | T], R, A, Type);
get_inline([{{inline, close}, _} | T], R, A, _) ->
    %% this is the markdown extension clause that takes an id in square brackets without
    %% any additional stuff as a valid id marker
    Id = make_plain_string(lists:reverse(A)),
    case lists:keyfind(Id, 1, R) of
        false  ->
            {T, lists:flatten([Id , $]])};
        {Id, {Url, Title}} ->
            Tag = {Url, Title, Id},
            {T, Tag}
    end;
get_inline([H | T], R, A, Type) ->
    get_inline(T, R, [H | A], Type).

get_id_diff(List) ->
    g_id_diff1(List, []).

g_id_diff1([], _Acc) ->
    normal;
g_id_diff1([{{inline, close}, _}| T], Acc) ->
    {lists:reverse(Acc), T};
g_id_diff1([H | T], Acc) ->
    g_id_diff1(T, [H | Acc]).

%% convert ascii into html characters
htmlencode(List) ->
    htmlencode(List, []).

htmlencode([], Acc) ->
    lists:flatten(lists:reverse(Acc));

htmlencode([$& | Rest], Acc) ->
    htmlencode(Rest, ["&amp;" | Acc]);
htmlencode([$< | Rest], Acc) ->
    htmlencode(Rest, ["&lt;" | Acc]);
htmlencode([$> | Rest], Acc) ->
    htmlencode(Rest, ["&gt;" | Acc]);
htmlencode([160 | Rest], Acc) ->
    htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) ->
    htmlencode(Rest, [Else | Acc]).

htmlchars(List) ->
    htmlchars1(List, []).

htmlchars1([], Acc) ->
    lists:flatten(lists:reverse(Acc));
htmlchars1([{tags, Tag} | T], Acc) ->
    %% tags are just wheeched out unescaped
    htmlchars1(T, [Tag | Acc]);
htmlchars1([?CR, ?LF | T], Acc) ->
    %% line ends are pushed to a space..
    htmlchars1(T, ["\n" | Acc]);
htmlchars1([?LF | T], Acc)  ->
    %% line ends are pushed to a space..
    htmlchars1(T, ["\n" | Acc]);
htmlchars1([?CR | T], Acc)  ->
    htmlchars1(T, ["\r" | Acc]);
htmlchars1([$\\, $*, $*, $* | T], A) ->
    %% there is a non-space filling white space represented by the atom 'none'
    %% which is created in the parser (NOT IN THE LEXER!) and which triggers
    %% emphasis or strong tags being turned on...
    htmlchars1(T, [$*, $*, $* | A]);
htmlchars1([$*, $*, $* | T], A) ->
    {T2, NewA} = superstrong(T, $*),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $*, $* | T], A) ->
    %% repeat for strong
    htmlchars1(T, [$*, $* | A]);
htmlchars1([$*, $* | T], A) ->
    {T2, NewA} = strong(T, $*),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $* | T], A) ->
    %% likewise for strong
    htmlchars1(T, [$* | A]);
htmlchars1([$* | T], A) ->
    {T2, NewA} = emphasis(T, $*),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $_, $_, $_ | T], A) ->
    %% and again for underscores
    htmlchars1(T, [$_, $_, $_ | A]);
htmlchars1([$_, $_, $_ | T], A) ->
    %% the none atom is the non-space filling whitespace
    {T2, NewA} = superstrong(T, $_),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $_, $_ | T], A) ->
    %% and strong
    %% and again for underscores
    htmlchars1(T, [$_, $_ | A]);
htmlchars1([$_, $_ | T], A) ->
    {T2, NewA} = strong(T, $_),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $_ | T], A) ->
    %% likewise for strong
    htmlchars1(T, [$_ | A]);
htmlchars1([$_ | T], A) ->
    {T2, NewA} = emphasis(T, $_),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$\\, $` | T], A) ->
    %% handle backtick escaping
    htmlchars1(T, [$` | A]);
htmlchars1([$`, $` | T], A) ->
    {T2, NewA} = dblcode(T),
    htmlchars1(T2, [NewA | A]);
htmlchars1([$` | T], A) ->
    {T2, NewA} = code(T),
    htmlchars1(T2, [NewA | A]);
htmlchars1([?COPY | T], A) ->
    htmlchars1(T, ["&copy;" | A]);
htmlchars1([?AMP | T], A)  ->
    htmlchars1(T, ["&amp;" | A]);
htmlchars1([$& | T], A) ->
    htmlchars1(T, ["&amp;" | A]);
htmlchars1([$< | T], A)  ->
    htmlchars1(T, ["&lt;" | A]);
htmlchars1([?NBSP | T], A)  ->
    htmlchars1(T, ["&nbsp;" | A]);
htmlchars1([?TAB | T], A) ->
    htmlchars1(T, ["    " | A]);
htmlchars1([none | T], A) ->
    htmlchars1(T, A);
htmlchars1([H | T], A) ->
    htmlchars1(T, [H | A]).

emphasis(List, Delim) ->
    interpolate(List, Delim, "em", "" ,[]).
strong(List, Delim) ->
    interpolate2(List, Delim, "strong", "", []).
superstrong(List, Delim) ->
    interpolate3(List, Delim, "strong", "em", "", []).
dblcode(List) ->
    {T, Tag} = interpolate2(List, $`, "code", "" ,[]),
    {T, "<pre>" ++ Tag ++ "</pre>"}.
code(List)  ->
    interpolateX(List, $`, "code", "", []).

%% pain in the arse - sometimes the closing tag should be preceded by
%% a "\n" and sometimes not in showdown.js
%% interpolate is for single delimiters...
interpolateX([], Delim, _Tag, _X, Acc) ->
    {[], [Delim] ++ htmlchars(lists:reverse(Acc))};
interpolateX([Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(lists:reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolateX([H | T], Delim, Tag, X, Acc) ->
    interpolateX(T, Delim, Tag, X, [H | Acc]).

interpolate([], Delim, _Tag, _X, Acc) ->
    {[], [Delim] ++ htmlchars(lists:reverse(Acc))};
interpolate([Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(lists:reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolate([H | T], Delim, Tag, X, Acc) ->
    interpolate(T, Delim, Tag, X, [H | Acc]).

%% interpolate two is for double delimiters...
interpolate2([], Delim, _Tag,  _X, Acc) ->
    {[], [Delim] ++ [Delim] ++ htmlchars(lists:reverse(Acc))};
interpolate2([Delim, Delim | T], Delim, Tag, X, Acc) ->
    {T,  "<" ++ Tag ++ ">" ++ htmlchars(lists:reverse(Acc)) ++ X ++
         "</" ++ Tag ++ ">"};
interpolate2([H | T], Delim, Tag, X, Acc) ->
    interpolate2(T, Delim, Tag, X, [H | Acc]).

%% interpolate three is for double delimiters...
interpolate3([], D, _Tag1, Tag2, _X, Acc)           ->
    {[], "<" ++ Tag2 ++ ">" ++ [D] ++ "</" ++ Tag2 ++ ">"
     ++ htmlchars(lists:reverse(Acc))};
interpolate3([D, D, D | T], D, Tag1, Tag2, _X, Acc) ->
    {T,  "<" ++ Tag1 ++ ">" ++  "<" ++ Tag2 ++ ">"
     ++ htmlchars(lists:reverse(Acc)) ++ "</" ++ Tag2 ++ ">"
     ++ "</" ++ Tag1 ++ ">"};
interpolate3([H | T], D, Tag1, Tag2, X, Acc) ->
    interpolate3(T, D, Tag1, Tag2, X, [H | Acc]).

make_img_tag(Url, Acc, Title) ->
    {tags, "<img src=\"" ++ Url ++ "\""
     ++ " alt=\"" ++ Acc ++ "\""
     ++ " title=\"" ++ Title ++ "\""
     ++ " />"}.
