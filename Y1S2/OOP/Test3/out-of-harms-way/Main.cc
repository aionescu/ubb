#include <QtWidgets/QApplication>
#include "Test.hh"
#include "GUI.hh"

int main(int argc, char** argv) {
  runAllTests();
  
  QApplication a{argc, argv};

  GUI gui;
  gui.show();

  return a.exec();
}