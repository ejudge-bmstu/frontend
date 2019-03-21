default: build

sources = src/Api.elm  src/Cred.elm  src/Main.elm  src/Page.elm  src/Ports.elm  \
	src/Route.elm  src/Session.elm  src/Tokens.elm  src/Username.elm  src/Viewer.elm \
	src/Page/Blank.elm  src/Page/Login.elm  src/Page/NotFound.elm  src/Page/Root.elm \
	src/Api/Endpoint.elm

build: sources  index.html
	elm make src/Main.elm --output=build/elm.js
	cp index.html build/index.html

clean:
	-rm -f -r build
