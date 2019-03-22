import os
from aiohttp import web

HOST = 'localhost'
PORT = 8080

INDEX_ROUTE = (os.path.abspath(os.path.dirname(__file__)) 
               + "/static/index.html")

routes = web.RouteTableDef()

async def get_index(request):
    return web.FileResponse(INDEX_ROUTE)

def init():
    app = web.Application()
    app.router.add_route('GET', '/{tail:.*}', get_index)
    return app

if __name__ == "__main__":
    web.run_app(init(), host=HOST, port = PORT)

