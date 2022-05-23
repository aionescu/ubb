#include <GL/glut.h>
#include <GLFW/glfw3.h>
#include <math.h>

double user_theta = 0;
double user_height = 0;

void drawSphere(double r, int lats, int longs) {
  int i, j;
  for (i = 0; i <= lats; i++) {
    double lat0 = M_PI * (-0.5 + (double)(i - 1) / lats);
    double z0 = sin(lat0);
    double zr0 = cos(lat0);
    double lat1 = M_PI * (-0.5 + (double)i / lats);
    double z1 = sin(lat1);
    double zr1 = cos(lat1);
    glBegin(GL_QUAD_STRIP);
    for (j = 0; j <= longs; j++) {
      double lng = 2 * M_PI * (double)(j - 1) / longs;
      double x = cos(lng);
      double y = sin(lng);
      glNormal3f(x * zr0, y * zr0, z0);
      glVertex3f(x * zr0, y * zr0, z0);
      glNormal3f(x * zr1, y * zr1, z1);
      glVertex3f(x * zr1, y * zr1, z1);
    }
    glEnd();
  }
}

void computeLocation() {
  double x = 10 * cos(user_theta); // my x-, y-, and z-coordinates
  double y = 2 * sin(user_theta);
  double z = user_height;
  double d = sqrt(x * x + y * y + z * z); // distance to origin
  glMatrixMode(GL_PROJECTION);            // Set projection parameters.
  glLoadIdentity();
  glFrustum(-d * 0.5, d * 0.5, -d * 0.5, d * 0.5, d - 1.1, d + 1.1);
  gluLookAt(x, y, z, 0, 0, 0, 0, 0, 1);
}

// Initializes information for drawing within OpenGL.
void init() {
  GLfloat sun_direction[] = {0.0, 2.0, -1.0, 1.0};
  GLfloat sun_intensity[] = {0.7, 0.7, 0.7, 1.0};
  GLfloat ambient_intensity[] = {0.3, 0.3, 0.3, 1.0};
  glClearColor(0.0, 0.0, 0.0, 0.0); // Set window color to black.
  computeLocation();
  //glEnable(GL_DEPTH_TEST); // Draw only closest surfaces
  glEnable(GL_LIGHTING);   // Set up ambient light.
  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambient_intensity);
  glEnable(GL_LIGHT0); // Set up sunlight.
  glLightfv(GL_LIGHT0, GL_POSITION, sun_direction);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, sun_intensity);
  glEnable(GL_COLOR_MATERIAL); // Configure glColor().
  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
}
// Draws the current image.
void draw() {
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // Clear window.
  glColor3f(1.0, 1.0, 1.0);
  glShadeModel(GL_SMOOTH);

  glRotatef(5, 0.0, 0.0, 1.0);
  glTranslatef(0.1, 0, 0);

  drawSphere(1.0, 20, 20);
  // glutSolidSphere(1.0, 20, 20);

  glFlush();
  glutSwapBuffers();
}
// Arranges that the window will be redrawn roughly every 40 ms.
void idle() {
  static int lastTime = 0;               // time of last redraw
  int time = glutGet(GLUT_ELAPSED_TIME); // current time
  if (lastTime == 0 || time >= lastTime + 40) {
    lastTime = time;

    glutPostRedisplay();
  }
}
// When window becomes visible, we want the window to
// continuously repaint itself.
void visible(int vis) { glutIdleFunc(vis == GLUT_VISIBLE ? idle : NULL); }
// Called when a "special" key is pressed
void special(int k, int x, int y) {
  switch (k) {
  case GLUT_KEY_UP:
    user_height += 0.1;
    break;
  case GLUT_KEY_DOWN:
    user_height -= 0.1;
    break;
  case GLUT_KEY_LEFT:
    user_theta += 0.1;
    break;
  case GLUT_KEY_RIGHT:
    user_theta -= 0.1;
    break;
  }
  computeLocation();
  glutPostRedisplay();
}
int main(int argc, char **argv) {
  glutInit(&argc, argv);
  glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);
  glutInitWindowSize(1080, 1080);
  glutCreateWindow("Sphere");
  init();
  glutDisplayFunc(draw);
  glutVisibilityFunc(visible);
  glutSpecialFunc(special);
  glutMainLoop();
  return 0;
}
