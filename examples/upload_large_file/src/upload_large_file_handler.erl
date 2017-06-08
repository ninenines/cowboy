%% Feel free to use, reuse and abuse the code in this file.

%% @doc Upload handler.
-module(upload_large_file_handler).

-export([init/2]).

init(Req, Opts) ->
	{Data, Req2} = do_init(Req),
	{ok, cowboy_req:reply(200, #{}, term_to_binary(Data), Req2), Opts}.

do_init(Req) ->
	{Data, Req2} = acc_multipart(Req, []),
		
	[{Headers, Body}] = Data,
	
	{_HeaderPart_1, HeaderPart_2 } = cow_multipart:parse_content_disposition(erlang:term_to_binary(Headers)),
	{<<"filename">>, FilenameBin} = lists:keyfind(<<"filename">>, 1, HeaderPart_2),
	FileName = erlang:binary_to_list(FilenameBin),
	
	%% Put the file into the current directory
	FileWriteRes = file:write_file(FileName, Body),
	
	error_logger:info_report(["Recived file has been saved", 
							  {headers, Headers},
							  {result, FileWriteRes},
							  {fileName, FileName},
							  {path, os:cmd("pwd")}
							  ]),
	
	{Data, Req2}.

acc_multipart(Req0, Acc) ->
    case cowboy_req:read_part(Req0) of
        {ok, Headers, Req1} ->
            {ok, Body, Req} = stream_body(Req1, <<>>),
            acc_multipart(Req, [{Headers, Body}|Acc]);
        {done, Req} ->
            {lists:reverse(Acc), Req}
    end.

stream_body(Req0, Acc) ->
    case cowboy_req:read_part_body(Req0) of
        {more, Data, Req} ->
            stream_body(Req, << Acc/binary, Data/binary >>);
        {ok, Data, Req} ->
            {ok, << Acc/binary, Data/binary >>, Req}
    end.

