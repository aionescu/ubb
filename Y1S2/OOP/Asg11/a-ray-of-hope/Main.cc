#include <fstream>
#include <iostream>
#include <utility>
#include <QApplication>
#include "Services.hh"
#include "GUI/GUI.hh"

std::pair<std::string, std::string> readConfigFilePaths() {
  std::ifstream config{"Config.txt"};

  std::string mainRepoFilePath, mylistFilePath;

  std::getline(config, mainRepoFilePath);
  std::getline(config, mylistFilePath);

  if (mylistFilePath != "in-memory" && mainRepoFilePath == "in-memory") {
    std::cout << "Mylist cannot out-persist the main repository." << std::endl;
    std::exit(1);
  }

  if (mainRepoFilePath == "in-memory")
    mainRepoFilePath = "";

  if (mylistFilePath == "in-memory")
    mylistFilePath = "";

  return {mainRepoFilePath, mylistFilePath};
}

int main(int argc, char *argv[]) {
  Services services;
  auto filePaths = readConfigFilePaths();

  services.setFilePath(filePaths.first);
  services.setServantTasksFilePath(filePaths.second);

  QApplication app{argc, argv};

  GUI gui{services};  
  gui.show();

  return app.exec();
}