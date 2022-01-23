from flask import abort, request, g, jsonify, url_for, Blueprint
from config import http_auth, http_token_auth, db, app
from models.user import User
from sqlalchemy.sql.functions import current_user

auth = Blueprint("auth", __name__)
global_user = None

@app.before_request
def before_request():
    g.user = current_user

@http_token_auth.verify_token
def verify_token(token):
  user_id = User.get_user_id_from_token(token)
  user = User.query.filter_by(id=user_id).first()
  if user:
    return True
  return False

@http_auth.verify_password
def verify_password(username_or_token, password):
  # first try to authenticate by token
  user = User.get_user_id_from_token(username_or_token)
  if not user:
    # try to authenticate with username/password
    user = User.query.filter_by(username = username_or_token).first()
    if not user or not user.verify_password(password):
      return False

  g.user = user
  g.user.id = user.id
  return True

@auth.route("/register", methods = ["POST"])
def register():
  username = request.json.get("username")
  password = request.json.get("password")

  if username is None or password is None:
    abort(400)

  if User.query.filter_by(username = username).first() is not None:
    print("User already exists!")
    abort(400)

  user = User(username = username)
  user.hash_password(password)

  db.session.add(user)
  db.session.commit()

  token = User.generate_jwt_from_id(user.id, 600000)

  return (
    jsonify({ "token": str(token, "ascii"), "duration": 600000 }),
    201
    #,{ "Location": url_for("user", id = user.id, _external = True, _method='GET') }
    # TODO: This throws an exception because it somehow does not find the URL
    # Error: werkzeug.routing.BuildError: Could not build url for endpoint 'user' ('GET') with values ['id']. Did you mean 'auth.get_user' instead?
  )

@auth.route("/user/<int:id>")
@http_auth.login_required
def get_user(id):
  user = User.query.get(id)
  if not user:
    abort(400)

  return jsonify({ "username": user.username })

@auth.route("/token")
@http_auth.login_required
def get_auth_token():
  token = g.user.generate_auth_token(60000)

  return jsonify({ "token": str(token, "ascii"), "duration": 600000 })
