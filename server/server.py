import os
from flask import Flask, send_file

HOST = 'localhost'
PORT = 8088

SCRIPT_ROUTE = os.path.abspath(os.path.dirname(__file__))
INDEX_FILE = SCRIPT_ROUTE + "/index.html"
STATIC_ROUTE = "static/"

app = Flask(__name__)


@app.route('/', defaults={'path': ''})
@app.route('/<path:path>')
def get_dir(path):
    if STATIC_ROUTE == path[:len(STATIC_ROUTE)]:
        return send_file(SCRIPT_ROUTE + "//" + path)
    else:
        return send_file(INDEX_FILE)


if __name__ == '__main__':
    app.run(debug=False, host=HOST, port=PORT)
