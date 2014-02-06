Multipart requests
==================

You can read and parse multipart messages using the
Req object directly.

Cowboy defines two functions that allows you to get
information about each part and read their contents.

Checking the content-type
-------------------------

While there is a variety of multipart messages, the
most common on the Web is `multipart/form-data`. It's
the type of message being sent when an HTML form
allows uploading files.

You can quickly figure out if a multipart message
has been sent by parsing the `content-type` header.

``` erlang
{ok, {<<"multipart">>, <<"form-data">>, _}, Req2}
    = cowboy_req:parse_header(<<"content-type">>, Req).
```

Reading a multipart message
---------------------------

To read a message you have to iterate over all its
parts. Then, for each part, you can inspect its headers
and read its body.

``` erlang
multipart(Req) ->
    case cowboy_req:part(Req) of
        {ok, _Headers, Req2} ->
            {ok, _Body, Req3} = cowboy_req:part_body(Req2),
            multipart(Req3);
        {done, Req2} ->
            Req2
    end.
```

Parts do not have a size limit. When a part body is
too big, Cowboy will return what it read so far and
allow you to continue if you wish to do so.

The function `cow_multipart:form_data/1` can be used
to quickly obtain information about a part from a
`multipart/form-data` message. This function will
tell you if the part is for a normal field or if it
is a file being uploaded.

This can be used for example to allow large part bodies
for files but crash when a normal field is too large.

``` erlang
multipart(Req) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            Req4 = case cow_multipart:form_data(Headers) of
                {data, _FieldName} ->
                    {ok, _Body, Req3} = cowboy_req:part_body(Req2),
                    Req3;
                {file, _FieldName, _Filename, _CType, _CTransferEncoding} ->
                    stream_file(Req2)
            end,
            multipart(Req4);
        {done, Req2} ->
            Req2
    end.

stream_file(Req) ->
    case cowboy_req:part_body(Req) of
        {ok, _Body, Req2} ->
            Req2;
        {more, _Body, Req2} ->
            stream_file(Req2)
    end.
```

By default the body chunk Cowboy will return is limited
to 8MB. This can of course be overriden.

Skipping unwanted parts
-----------------------

If you do not want to read a part's body, you can skip it.
Skipping is easy. If you do not call the function to read
the part's body, Cowboy will automatically skip it when
you request the next part.

The following snippet reads all part headers and skips
all bodies:

``` erlang
multipart(Req) ->
    case cowboy_req:part(Req) of
        {ok, _Headers, Req2} ->
            multipart(Req2);
        {done, Req2} ->
            Req2
    end.
```

Similarly, if you start reading the body and it ends up
being too big, you can simply continue with the next part,
Cowboy will automatically skip what remains.

And if you started reading the message but decide that you
do not need the remaining parts, you can simply stop reading
entirely and Cowboy will automatically figure out what to do.
