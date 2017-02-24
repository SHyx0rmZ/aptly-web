# aptly-web

This is a rudimentary [Elm](http://elm-lang.org/)-based web interface for [aptly](https://www.aptly.info/)'s API.

## Setup

1. Ensure aptly is serving its API. (Run `aptly serve`).
2. Modify `init` in `src/Main.elm`, so `config` points to your aptly installation.
3. Execute `build.sh`.
4. Serve `index.html` over HTTP. You may have to work some magic related to [CORS](https://en.wikipedia.org/wiki/Cross-origin_resource_sharing).
