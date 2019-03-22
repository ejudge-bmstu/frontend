elm make src/Main.elm --output=build/elm.js
cp -r static build
cd build
python -m SimpleHTTPServer 8081
