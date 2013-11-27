Using cookies
=============

Cookies are a mechanism allowing applications to maintain
state on top of the stateless HTTP protocol.

Cowboy provides facilities for handling cookies. It is highly
recommended to use them instead of writing your own, as the
implementation of cookies can vary greatly between clients.

Cookies are stored client-side and sent with every subsequent
request that matches the domain and path for which they were
stored, including requests for static files. For this reason
they can incur a cost which must be taken in consideration.

Also consider that, regardless of the options used, cookies
are not to be trusted. They may be read and modified by any
program on the user's computer, but also by proxies. You
should always validate cookie values before using them. Do
not store any sensitive information in cookies either.

When explicitly setting the domain, the cookie will be sent
for the domain and all subdomains from that domain. Otherwise
the current domain will be used. The same is true for the
path.

When the server sets cookies, they will only be available
for requests that are sent after the client receives the
response.

Cookies are sent in HTTP headers, therefore they must have
text values. It is your responsibility to encode any other
data type. Also note that cookie names are de facto case
sensitive.

Cookies can be set for the client session (which generally
means until the browser is closed), or it can be set for
a number of seconds. Once it expires, or when the server
says the cookie must exist for up to 0 seconds, the cookie
is deleted by the client. To avoid this while the user
is browsing your site, you should set the cookie for
every request, essentially resetting the expiration time.

Cookies can be restricted to secure channels. This typically
means that such a cookie will only be sent over HTTPS,
and that it will only be available by client-side scripts
that run from HTTPS webpages.

Finally, cookies can be restricted to HTTP and HTTPS requests,
essentially disabling their access from client-side scripts.

Setting cookies
---------------

By default, cookies you set are defined for the session.

``` erlang
SessionID = generate_session_id(),
Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, [], Req).
```

You can also make them expire at a specific point in the
future.

``` erlang
SessionID = generate_session_id(),
Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, [
    {max_age, 3600}
], Req).
```

You can delete cookies that have already been set. The value
is ignored.

``` erlang
Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, <<>>, [
    {max_age, 0}
], Req).
```

You can restrict them to a specific domain and path.
For example, the following cookie will be set for the domain
`my.example.org` and all its subdomains, but only on the path
`/account` and all its subdirectories.

``` erlang
Req2 = cowboy_req:set_resp_cookie(<<"inaccount">>, <<"1">>, [
    {domain, "my.example.org"},
    {path, "/account"}
], Req).
```

You can restrict the cookie to secure channels, typically HTTPS.

``` erlang
SessionID = generate_session_id(),
Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, [
    {secure, true}
], Req).
```

You can restrict the cookie to client-server communication
only. Such a cookie will not be available to client-side scripts.

``` erlang
SessionID = generate_session_id(),
Req2 = cowboy_req:set_resp_cookie(<<"sessionid">>, SessionID, [
    {http_only, true}
], Req).
```

Cookies may also be set client-side, for example using
Javascript.

Reading cookies
---------------

As we said, the client sends cookies with every request.
But unlike the server, the client only sends the cookie
name and value.

You can read the value of a cookie.

``` erlang
{CookieVal, Req2} = cowboy_req:cookie(<<"lang">>, Req).
```

You can also get a default value returned when the cookie
isn't set.

``` erlang
{CookieVal, Req2} = cowboy_req:cookie(<<"lang">>, Req, <<"fr">>).
```

And you can obtain all cookies at once as a list of
key/value tuples.

``` erlang
{AllCookies, Req2} = cowboy_req:cookies(Req).
```
