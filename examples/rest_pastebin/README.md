Cowboy Rest Hello World
=======================

To compile this example you need rebar in your PATH.

Type the following command:
```
$ rebar get-deps compile
```

You can then start the Erlang node with the following command:
```
./start.sh
```

Then run any given command or point your browser to the indicated URL.

Examples
--------

To upload something to the paste application, you can use curl like:
```
<command> | curl -i --data-urlencode paste@- localhost:8080
```
or to upload my_file:
```
curl -i --data-urlencode paste@my_file localhost:8080
```

The URL of your data will be in the location header. Alternately, you can visit
http://localhost:8080 with your favorite web browser and submit your paste via
the form.

Code that has been pasted can be highlighted with ?lang=<language> option if
you have [highlight](http://www.andre-simon.de/doku/highlight/en/highlight.html)
installed (although pygments or any other should work just fine). For example:
```
curl -i --data-urlencode paste@priv/index.html localhost:8080
curl <url from location header>
```

Will show the text of the html file. If your terminal supports color
sequences and highlight is installed:
```
curl <url from location header>?lang=html
```

Will show a syntax highlighted version of the source file. If you open the
same URL in your web browser and your web browser tells cowboy that it prefers
html files, you will see the file highlighted with html/css markup. Firefox is
known to work.

