#include <QtWidgets/QApplication>
#include "Services.hh"
#include "GUI.hh"
#include "Test.hh"

int main(int argc, char** argv) {
  test_Service_tryAddStar();
  
  auto services = new Service;
  services->loadFromFile<Astronomer>("../Astronomers.txt");
  services->loadFromFile<Star>("../Stars.txt");
  
  QApplication app{argc, argv};

  for (auto astronomer : services->getAllData<Astronomer>()) {
    auto astronomerWindow = new AstronomerWindow{services, astronomer};
    astronomerWindow->show();
  }

  auto result = app.exec();

  services->sortByConstellation();
  services->saveToFile<Star>("../Stars.txt");

  return result;
}