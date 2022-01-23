from config import db, app
from werkzeug.security import generate_password_hash, check_password_hash
import jwt
from time import time

class User(db.Model):
  __tablename__ = "users"
  id = db.Column(db.Integer, primary_key=True)
  username = db.Column(db.String(32), index=True)
  password = db.Column(db.String(128))

  def hash_password(self, password):
    self.password = generate_password_hash(password)

  def verify_password(self, password):
    return check_password_hash(self.password, password)

  def generate_auth_token(self, expires_in = 600000):
    return User.generate_jwt_from_id(self.id, expires_in)

  @staticmethod
  def generate_jwt_from_id(id, expires_in = 600000):
    return jwt.encode(
      { "id": id, "exp": time() + expires_in },
      app.config["SECRET_KEY"],
      algorithm = "HS256"
    )

  @staticmethod
  def get_user_id_from_token(token):
    try:
      data = jwt.decode(token, app.config["SECRET_KEY"], algorithms = ["HS256"])
      return data["id"]
    except Exception as e:
      print(e)
      return None
