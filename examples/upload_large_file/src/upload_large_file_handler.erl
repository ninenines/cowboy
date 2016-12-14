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
	
	{_, << FileData/binary >>} = lists:keyfind(<<"content-disposition">>, 1, Headers),
	
	%% Note: The FileData looks like this: "form-data; name=\"inputfile\"; filename=\"test_upload.txt\""
	[_FormData, _Name, FileNameTmp] = string:tokens(erlang:binary_to_list(FileData), ";"),
	[_, FileNameTmp2] = string:tokens(FileNameTmp, "="),
	FileName = string:strip(FileNameTmp2, both, $"),
		
	%% Put the file into the current directory
	FileWriteRes = file:write_file(FileName, Body),
	
	error_logger:info_report(["Recived file has been saved", 
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

