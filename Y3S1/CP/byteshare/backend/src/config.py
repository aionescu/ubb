from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from flask_httpauth import HTTPBasicAuth
from flask_httpauth import HTTPTokenAuth

# Init
app = Flask("ByteShare API")
app.config["SECRET_KEY"] = "There’s a lesson here, and I’m not going to be the one to figure it out"
app.config["SQLALCHEMY_DATABASE_URI"] = "postgresql://avffogivlqbvbo:e24fbc2a7253d5eeac750b33c03ffd2e47376c2c0ad4479cc22212da86eff0da@ec2-34-250-16-127.eu-west-1.compute.amazonaws.com:5432/d9arigtorjpif9"
app.config["SQLALCHEMY_COMMIT_ON_TEARDOWN"] = True
app.config["SQLALCHEMY_TRACK_MODIFICATIONS"] = False

# Extensions
db = SQLAlchemy(app)
http_auth = HTTPBasicAuth()
http_token_auth = HTTPTokenAuth()
