elm make src/Main.elm --output=build/elm.js
cp index.html build/index.html
cd build
python -m SimpleHTTPServer 8081
