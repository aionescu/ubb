#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <iostream>
#include <fstream>
#include<vector>

#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "model.hh"
#include "mesh.hh"
#include "obj.hh"
#include "scene.hh"

#define STB_IMAGE_IMPLEMENTATION
#include <stb/stb_image.h>

// functii ce creaza shaderele
GLuint createProgram(const char *VSFile, const char *FSFile);
std::string readFile(const char *filePath);

// functie ce creaza scena
// aceasta functie trebuie rescrisa de voi
Scene createScene(GLuint programShadere);

// functii ce initializeaza fereastra si
// proceseaza intrarile de la mouse si tastatura
GLFWwindow *initializari();
void framebuffer_size_callback(GLFWwindow *window, int width, int height);
void mouse_callback(GLFWwindow *window, double xpos, double ypos);
void scroll_callback(GLFWwindow *window, double xoffset, double yoffset);
void processInput(GLFWwindow *window, Scene& s);

// configurari
const unsigned int SCR_WIDTH = 1920;
const unsigned int SCR_HEIGHT = 1080;

// camera
glm::vec3 cameraPos = glm::vec3(10, 0.5, 10);
glm::vec3 cameraFront = glm::vec3(0, -1, -1);
glm::vec3 cameraUp = glm::vec3(0, 1, 0);

bool firstMouse = true;

// yaw is initialized to -90.0 degrees since a yaw of 0.0
// results in a direction vector pointing to the right so we
// initially rotate a bit to the left.
float yaw = -90.0f;
float pitch = 0.0f;
float lastX = 800.0f / 2.0;
float lastY = 600.0 / 2.0;
float fov = 45.0f;

// timing
float deltaTime = 0.0f; // time between current frame and last frame
float lastFrame = 0.0f;

int get_rand(int min, int max) { return rand() % (max - min + 1) + min; }

int main() {
  // initializam fereastra
  GLFWwindow *fereastra = initializari();

  // cream shaderele si folosim programul creat
  // (avand o scena simpla folosim un singur program)
  GLuint program = createProgram("shaders/obiect1.vert", "shaders/obiect1.frag");
  glUseProgram(program);

  // cream scena
  Scene s = createScene(program);

  while (!glfwWindowShouldClose(fereastra)) {
    // per-frame time logic
    // --------------------
    float currentFrame = static_cast<float>(glfwGetTime());
    deltaTime = currentFrame - lastFrame;
    lastFrame = currentFrame;

    // procesam intrarile ferestrei: din taste si/sau mouse
    processInput(fereastra, s);

    // trimitem matricea projection catre shadere
    glm::mat4 projection = glm::perspective(
        glm::radians(fov), (float)SCR_WIDTH / (float)SCR_HEIGHT, 0.1f, 100.0f);
    glUniformMatrix4fv(glGetUniformLocation(program, "projection"), 1, GL_FALSE, &projection[0][0]);

    // camera/view transformation
    glm::mat4 view = glm::lookAt(cameraPos, cameraPos + cameraFront, cameraUp);
    glUniformMatrix4fv(glGetUniformLocation(program, "view"), 1, GL_FALSE, &view[0][0]);

    glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    s.draw();

    glfwSwapBuffers(fereastra);
    glfwPollEvents();
  }
  glfwTerminate();
  return 0;
}

std::string rand_crate() {
  switch (get_rand(0, 2)) {
  case 0:
    return "assets/crates/crate0.obj";
  case 1:
    return "assets/crates/crate1.obj";
  default:
    return "assets/crates/crate2.obj";
  }
}

Scene createScene(GLuint program) {
  srand(time(0));
  Scene s{program};

  auto room_size = 20;
  auto room_height = 2;
  auto crate_stack = 5;
  auto shelves = 6;
  auto cones = 5;

  for (int i = 0; i < room_size; ++i)
    for (int j = 0; j < room_size; ++j)
      s.addCached("assets/room/tiles.obj", {(float)i, 0.0, (float)j}, {0, 1, 1, 0}, 1);

  for (int i = 0; i < room_size; ++i) {
    for (int j = 0; j < room_height; ++j) {
      s.addCached("assets/room/brickwall.obj", {-0.5f, 0.5f + j, (float)i}, {90, 1, 0, 0}, 1);
      s.addCached("assets/room/brickwall.obj", {19.5f, 0.5f + j, (float)i}, {90, 1, 0, 0}, 1);
      s.addCached("assets/room/brickwall.obj", {(float)i, 0.5f + j, -0.5f}, {90, 0, 1, 0}, 1);
      s.addCached("assets/room/brickwall.obj", {(float)i, 0.5f + j, 19.5f}, {90, 0, 1, 0}, 1);
    }
  }

  for (int h = crate_stack; h > 0; --h)
    for (int i = 0; i < h; ++i)
      for (int j = 0; j < h; ++j)
        if (i + j < h)
          s.addCached(rand_crate(), {2.0f + i, 0.5f + (crate_stack - h), 2.0f + j}, 0, 0.5f);

  for (int i = 0; i < shelves; ++i)
    s.addCached("assets/shelf/shelf.obj", {2 + 1.55f * i, 0, 18.5f}, {90, 0, 1, 0}, 0.3f);

  auto dist = 0.0f;
  auto delta = 2.0f;
  for (auto i = 0; i < cones; ++i) {
    s.addCached("assets/cone/cone.obj", {17.5f, 0, 17.5f - dist}, 0, 0.5f - 0.1f * i);
    dist += delta;
    delta -= 0.4f;
  }

  s.addCached("assets/crates/crate1.obj", {2.25f, 1.55f, 17}, 0, 0.2f);
  s.addCached("assets/crates/crate2.obj", {1.85f, 1.55f, 17}, 0, 0.2f);

  return s;
}

GLFWwindow *initializari() {
  // functia initializeaza bibliotecile glfw si glad, creaza o fereastra si
  // o ataseaza  unui context OpenGL

  // initializam glfw
  glfwInit();

  glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
  glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
  glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

  // cream fereastra
  GLFWwindow* window = glfwCreateWindow(SCR_WIDTH, SCR_HEIGHT, "scena_01", NULL, NULL);
  if (window == NULL) {
    std::cout << "Failed to create GLFW window" << std::endl;
    glfwTerminate();
  }
  // facem ca aceasta fereastra sa fie contextul curent

  // atasam fereastra contextului opengl
  // glfwMakeContextCurrent(window);

  glfwMakeContextCurrent(window);
  glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);
  glfwSetCursorPosCallback(window, mouse_callback);
  glfwSetScrollCallback(window, scroll_callback);

  // tell GLFW to capture our mouse
  glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);

  if (!gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
    std::cout << " nu s-a initializat biblioteca GLAD!";

  // configure global opengl state
  // -----------------------------
  glEnable(GL_DEPTH_TEST);

  return window;
}

// process all input: query GLFW whether relevant keys are pressed/released this
// frame and react accordingly
// ---------------------------------------------------------------------------------------------------------
void processInput(GLFWwindow *window, Scene& s) {
  if (glfwGetKey(window, GLFW_KEY_Q) == GLFW_PRESS)
    glfwSetWindowShouldClose(window, true);
  if (glfwGetKey(window, GLFW_KEY_E) == GLFW_PRESS)
    std::cout << "(" << cameraPos.x << ", " << cameraPos.y << ", " << cameraPos.z << ")" << std::endl;

  float cameraSpeed = static_cast<float>(2.5 * deltaTime);

  if (glfwGetKey(window, GLFW_KEY_W) == GLFW_PRESS)
    cameraPos += cameraSpeed * cameraFront;
  if (glfwGetKey(window, GLFW_KEY_S) == GLFW_PRESS)
    cameraPos -= cameraSpeed * cameraFront;
  if (glfwGetKey(window, GLFW_KEY_SPACE) == GLFW_PRESS)
    cameraPos += cameraSpeed * cameraUp;
  if (glfwGetKey(window, GLFW_KEY_LEFT_SHIFT) == GLFW_PRESS)
    cameraPos -= cameraSpeed * cameraUp;
  if (glfwGetKey(window, GLFW_KEY_A) == GLFW_PRESS)
    cameraPos -= glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed;
  if (glfwGetKey(window, GLFW_KEY_D) == GLFW_PRESS)
    cameraPos += glm::normalize(glm::cross(cameraFront, cameraUp)) * cameraSpeed;

  cameraPos.y = std::max(cameraPos.y, 0.5f);
}

// glfw: whenever the window size changed (by OS or user resize) this callback
// function executes
// ---------------------------------------------------------------------------------------------
void framebuffer_size_callback(GLFWwindow *window, int width, int height) {
  // make sure the viewport matches the new window dimensions; note that width
  // and height will be significantly larger than specified on retina displays.
  glViewport(0, 0, width, height);
}

// glfw: whenever the mouse moves, this callback is called
// -------------------------------------------------------
void mouse_callback(GLFWwindow *window, double xposIn, double yposIn) {
  float xpos = static_cast<float>(xposIn);
  float ypos = static_cast<float>(yposIn);

  if (firstMouse) {
    lastX = xpos;
    lastY = ypos;
    firstMouse = false;
  }

  float xoffset = xpos - lastX;
  float yoffset = lastY - ypos; // reversed since y-coordinates go from bottom to top
  lastX = xpos;
  lastY = ypos;

  float sensitivity = 0.1f; // change this value to your liking
  xoffset *= sensitivity;
  yoffset *= sensitivity;

  yaw += xoffset;
  pitch += yoffset;

  // make sure that when pitch is out of bounds, screen doesn't get flipped
  if (pitch > 89.0f)
    pitch = 89.0f;
  if (pitch < -89.0f)
    pitch = -89.0f;

  glm::vec3 front;
  front.x = cos(glm::radians(yaw)) * cos(glm::radians(pitch));
  front.y = sin(glm::radians(pitch));
  front.z = sin(glm::radians(yaw)) * cos(glm::radians(pitch));
  cameraFront = glm::normalize(front);
}

// glfw: whenever the mouse scroll wheel scrolls, this callback is called
// ----------------------------------------------------------------------
void scroll_callback(GLFWwindow *window, double xoffset, double yoffset) {
  fov -= (float)yoffset;
  if (fov < 1.0f)
    fov = 1.0f;
  if (fov > 45.0f)
    fov = 45.0f;
}

GLuint createProgram(const char *VSFile, const char *FSFile) {
  // functia creaza un program shader folosind ca fisiere sursa VSFile si
  // FSFile IN: numele fisierelor sursa OUT: aliasul programului shader

  GLuint vertexShader, fragmentShader, shaderProgram;
  int success;
  char infoLog[512];

  vertexShader = glCreateShader(GL_VERTEX_SHADER);

  if (0 == vertexShader) {
    std::cout << "Error creating vertex shader." << std::endl;
    exit(1);
  }

  std::string shaderCode = readFile(VSFile);
  const char *codeArray = shaderCode.c_str();
  glShaderSource(vertexShader, 1, &codeArray, NULL);

  glCompileShader(vertexShader);

  // verficam daca s-a reusit compilarea codului

  glGetShaderiv(vertexShader, GL_COMPILE_STATUS, &success);
  if (!success) {
    glGetShaderInfoLog(vertexShader, 512, NULL, infoLog);
    std::cout << "ERROR::SHADER::VERTEX::COMPILATION_FAILED\n" << infoLog << std::endl;
  }

  fragmentShader = glCreateShader(GL_FRAGMENT_SHADER);
  if (0 == fragmentShader) {
    std::cout << "Error creating fragment shader." << std::endl;
    exit(1);
  }

  shaderCode = readFile(FSFile);
  codeArray = shaderCode.c_str();
  glShaderSource(fragmentShader, 1, &codeArray, NULL);

  glCompileShader(fragmentShader);

  // se verifica compilarea codului

  glGetShaderiv(fragmentShader, GL_COMPILE_STATUS, &success);
  if (!success) {
    glGetShaderInfoLog(fragmentShader, 512, NULL, infoLog);
    std::cout << "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED\n" << infoLog << std::endl;
  }

  // cream programul

  shaderProgram = glCreateProgram();
  glAttachShader(shaderProgram, vertexShader);
  glAttachShader(shaderProgram, fragmentShader);
  glLinkProgram(shaderProgram);

  // se verifica procesul de link

  glGetProgramiv(shaderProgram, GL_LINK_STATUS, &success);
  if (!success) {
    glGetProgramInfoLog(shaderProgram, 512, NULL, infoLog);
    std::cout << "ERROR::SHADER::PROGRAM::LINKING_FAILED\n" << infoLog << std::endl;
  }
  glDeleteShader(vertexShader);
  glDeleteShader(fragmentShader);

  return shaderProgram;
}

std::string readFile(const char *filePath) {
  std::string content;
  std::ifstream fileStream(filePath, std::ios::in);

  if (!fileStream.is_open()) {
    std::cerr << "Could not read file " << filePath;
    std::cerr << ". File does not exist." << std::endl;
    return "";
  }

  std::string line = "";
  while (!fileStream.eof()) {
    std::getline(fileStream, line);
    content.append(line + "\n");
  }

  fileStream.close();
  return content;
}
