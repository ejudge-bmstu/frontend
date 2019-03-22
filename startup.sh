rm -r build
cp -ra static build
elm make src/Main.elm --output=build/elm.js
cd build
http-server
