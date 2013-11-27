REST pastebin example
=====================

To try this example, you need GNU `make` and `git` in your PATH.

To build the example, run the following command:

``` bash
$ make
```

To start the release in the foreground:

``` bash
$ ./_rel/bin/rest_pastebin_example console
```

Then point your browser at [http://localhost:8080](http://localhost:8080).

Usage
-----

To upload something to the paste application, you can use `curl`:

``` bash
$ <command> | curl -i --data-urlencode paste@- localhost:8080
```

Or, to upload the file `my_file`:

``` bash
curl -i --data-urlencode paste@my_file localhost:8080
```

The URL of your data will be in the location header. Alternately, you can visit
http://localhost:8080 with your favorite web browser and submit your paste via
the form.

Code that has been pasted can be highlighted with ?lang=<language> option if
you have [highlight](http://www.andre-simon.de/doku/highlight/en/highlight.html)
installed (although `pygments` or any other should work just fine).

This will show the contents of the HTML file:

``` bash
curl -i --data-urlencode paste@priv/index.html localhost:8080
curl <url from location header>
```

If your terminal supports color sequences and `highlight` is installed,
the following command will show the same contents but with HTML syntax
highlighting.

``` bash
curl <url from location header>?lang=html
```

If you open the same URL in your web browser and your web browser tells
Cowboy that it prefers HTML files, you will see the file highlighted
with special HTML markup and CSS. Firefox is known to work.
