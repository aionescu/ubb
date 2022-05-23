#ifndef OBJ_HH
#define OBJ_HH

#include <iostream>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "model.hh"

struct Location {
  float x, y, z;

  Location(): Location{0} {}
  Location(float f): x(f), y(f), z(f) {}
  Location(float x, float y, float z): x(x), y(y), z(z) {}
};

struct Rotation {
  float unghi, x, y, z;

  Rotation(): Rotation{0} {}
  Rotation(float f): unghi(f), x(f), y(f), z(f) {}
  Rotation(float unghi, float x, float y, float z): unghi(unghi), x(x), y(y), z(z) {}
};

struct Scaling {
  float x, y, z;

  Scaling(): Scaling{0} {}
  Scaling(float f): x(f), y(f), z(f) {}
  Scaling(float x, float y, float z): x(x), y(y), z(z) {}
};

class Obj {
public:
  Model model;

  std::vector<Rotation> rot;
  Scaling scal;
  Location loc;

  GLuint shaderProgram;

  Obj(Model model, Location l, Rotation r, Scaling s, GLuint& shaderProgram)
  : model{model}, loc{l}, rot{{r}}, scal{s}, shaderProgram{shaderProgram} {}

  void draw() {
    auto m = glm::translate(glm::mat4(1.0f), glm::vec3(loc.x, loc.y, loc.z));

    for (auto& r: rot)
      if (r.unghi != 0)
        m = glm::rotate(m, glm::radians(r.unghi), glm::vec3(r.x, r.y, r.z));

    m = glm::scale(m, glm::vec3(scal.x, scal.y, scal.z));
    glUniformMatrix4fv(glGetUniformLocation(shaderProgram, "model"), 1, GL_FALSE, &m[0][0]);

    model.draw(shaderProgram);
  }
};

#endif // OBJ_HH
