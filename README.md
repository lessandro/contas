# Contas

Roommate bill splitter.

Screenshot: http://i.imgur.com/jukaf.png

## Front-end

The front-end is a single-page app written in [LiveScript](http://livescript.net), using the [bootstrap](http://twitter.github.com/bootstrap/) framework.
Any change to the page is immediately saved to the back-end, and it is possible to navigate back in time to an arbitrary version of the data.

### Dependencies

- [LiveScript](http://livescript.net)
- [browserify](http://browserify.org)

### Instructions

1. Edit src/main.ls. You might be interested in changing `backend-url`, `names` and `month-names`.
2. Run `make`.
3. Copy www/ to your server.

## Back-end

For the back-end I wrote a simple HTTP server in Haskell (inspired by rephttpd). Initially I used CouchDB to persist the data but since I was unable to install the couchdb cabal package on Debian 6, I eventually replaced it with my own database.

The database (see LastDB.hs) is a ridiculously simple append-only key-value store. Each entry is a line in a file. Newlines are not supported in an entry, which is not a problem as I'm only storing JSON-serialized items. The key to each entry is the offset in the file to end of that line, and each read operation returns the key to the previous line.

As such, all operations (getThing, getLastThing, appendThing) run in constant time w.r.t the number of items in the database.

### Dependencies

- [Haskell Platform](http://www.haskell.org/platform/)

### Instructions

1. Edit the HTTP port in `Httpd.hs`, and the database file in `LastDB.hs`.
2. `cabal install`
3. `~/.cabal/bin/contas`
