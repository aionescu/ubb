from os import environ
from config import app
from apis.auth import *
from apis.files import *
from flask_cors import CORS

def main():
  environ["FLASK_ENV"] = "development"

  app.register_blueprint(auth)
  app.register_blueprint(files)
  CORS(app)

  app.run(host = "localhost", port = 5000, debug = True)

if __name__ == "__main__":
  main()
