from flask import request, g, jsonify, Response, Blueprint
from config import http_auth, http_token_auth
from models.file import File

from models.user import User

files = Blueprint("files", __name__)

def getUserId(request):
  return User.get_user_id_from_token(request.headers["Authorization"].split(" ")[1])

@files.route("/files")
@http_token_auth.login_required
def get_files():
  return jsonify({ "files": File.for_user(getUserId(request)) })

@files.route("/files/save", methods = ["POST"])
@http_token_auth.login_required
def save_file():

  user_id = getUserId(request)

  file = File(
    ownerid = user_id,
    filename = request.json.get("filename"),
    contents = request.json.get("contents"),
    filetype = request.json.get("filetype")
  )

  File.create_or_update(file)
  return Response(status = 200)

@files.route("/files/delete", methods = ["POST"])
@http_token_auth.login_required
def delete_file():
  filename = request.json.get("filename")
  ownerid = getUserId(request)

  if File.delete(filename, ownerid):
    return Response(status = 200)
  else:
    return Response(status = 400)

@files.route("/files/rename", methods = ["POST"])
@http_token_auth.login_required
def rename_file():
  filename = request.json.get("filename")
  new_filename = request.json.get("newFilename")
  ownerid = getUserId(request)

  if File.rename(ownerid, filename, new_filename):
    return Response(status = 200)
  else:
    return Response(status = 400)

@files.route("/files/compile", methods = ["POST"])
@http_token_auth.login_required
def compile_file():
  filename = request.json.get("filename")
  ownerid = getUserId(request)

  return jsonify({"result": File.compile(filename, ownerid)})
