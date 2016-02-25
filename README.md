# simple-chat-client

This is my first PureScript program, something simple for me to play around
with.  In particular, I am interested in
[Halogen](https://github.com/slamdata/purescript-halogen) and WebSockets.

I decided to make a client for the Haskell `websockets` library's nice little
[example chat server](https://jaspervdj.be/websockets/example.html).  I took
the code from that example, added project files such that I could build it with
Stack, and put it in the `server/` directory in this repo.

I instantiated this from
[purescript-halogen-template](https://github.com/slamdata/purescript-halogen-template)
This is a template for starting a fresh project using the library for
declarative user interfaces.

## Running the Server

The server is a Haskell program.  You'll need [Stack](http://haskellstack.org) to build it.

``` shell
cd server
stack install
stack exec simple-chat-server-exe
```
It'll echo chat traffic to stdout.

## Running the Client

Build it:

``` shell
npm run build
```

Though I mainly just edit the code in vim and run this in a tmux split:

``` shell
npm run watch
```
Now you can just load the page via file:// from the project directory.  There's
probably better ways, I'm new to PureScript.

