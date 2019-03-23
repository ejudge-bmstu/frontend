rm -rf build
mkdir -p build/static
cp static/index.html build/index.html
cp static/styles.css build/static/styles.css
cp server/server.py build/server.py
elm make src/Main.elm --output=build/static/elm.js
cd build
python3 server.py
