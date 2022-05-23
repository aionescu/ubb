#ifndef SCENE_HH
#define SCENE_HH

#include <iostream>
#include <map>
#include <algorithm>
#include "obj.hh"

struct Bounds {
  Location min, max;
};

class Scene {
public:
  Scene(GLuint program) : shaderProgram{program} {}

  void addCached(std::string path, Location l, Rotation r, Scaling s) {
    if (!modelCache.count(path))
      modelCache.insert({path, Model{path}});

    objs.push_back(Obj{modelCache.at(path), l, r, s, shaderProgram});
  }

  void draw() {
    for (auto& o: objs)
      o.draw();
  }

private:
  GLuint shaderProgram;
  std::map<std::string, Model> modelCache;
  std::vector<Obj> objs;
};

#endif // SCENE_HH
