#include <QApplication>
#include "gui.hh"

int main(int argc, char** argv) {
  QApplication app{argc, argv};
  GUI gui;

  gui.show();
  return app.exec();
}
