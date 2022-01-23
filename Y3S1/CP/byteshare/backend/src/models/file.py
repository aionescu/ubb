from flask.json import JSONEncoder
from config import db
import subprocess
import os

class File(db.Model):
  __tablename__ = "files"
  id = db.Column(db.Integer, primary_key=True)
  ownerid = db.Column(db.Integer)
  filename = db.Column(db.String(32))
  filetype = db.Column(db.String(32))
  contents = db.Column(db.String(128))

  def to_json(self):
    return { "filename": self.filename, "contents": self.contents, "filetype": self.filetype }

  @staticmethod
  def for_user(id):
    files = File.query.filter_by(ownerid = id).all()
    files_json = list(map(lambda f: f.to_json(), files))

    return files_json

  @staticmethod
  def create_or_update(file):
    existing = File.query.filter_by(ownerid = file.ownerid, filename = file.filename).first()

    if existing:
      existing.contents = file.contents
    else:
      db.session.add(file)

    db.session.commit()

  @staticmethod
  def rename(ownerid, filename, new_filename):
    existing = File.query.filter_by(ownerid=ownerid, filename=filename).first()

    if existing:
      existing.filename = new_filename
      db.session.commit()
      return True
    return False

  @staticmethod
  def delete(filename, ownerid):
    file = File.query.filter_by(ownerid = ownerid, filename = filename).first()

    if file:
      db.session.delete(file)
      db.session.commit()
      return True

    return False

  @staticmethod
  def compile(filename, ownerid):
    commands = {
      "py": "python3 {}",
      "js": "node {}",
      "c": "gcc {} && ./a.out",
      "cpp": "g++ {} && ./a.out",
      "cs": "mcs -out:a.exe {} && mono a.exe",
      "rb": "ruby {}",
      "java": "javac {} && java {}"
    }

    file = File.query.filter_by(ownerid=ownerid, filename=filename).first()

    filename, file_extension = filename.split('.')
    temp_compile_folder_path = "temp_compile/" + str(ownerid) + "_" + filename + "/"
    temp_file_path = temp_compile_folder_path + file.filename
    os.mkdir(temp_compile_folder_path)

    temp_file = open(temp_file_path, "a")
    temp_file.write(file.contents)
    temp_file.close()
    
    if file_extension != "java":
      p = subprocess.Popen(commands[file_extension].format(temp_file_path), shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    else:
      os.chdir(temp_compile_folder_path)
      p = subprocess.Popen(commands[file_extension].format(file.filename, filename), shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
      os.chdir("../..")

    result = b""
    for line in p.stdout.readlines():
      result += line
    
    os.system("rm -rf " + temp_compile_folder_path)
    return result.decode("utf-8")
