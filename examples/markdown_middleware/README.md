Cowboy Middleware
=================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Cowboy will serve all the files you put in the priv/ directory. If you request
a .html file that has corresponding .md file that has been modified more
recently than the .html file, the markdown file will be converted to HTML and
served by Cowboy.

HTML5 Video Example
-------------------

Open http://localhost:8080/video.html in your favorite browser.
