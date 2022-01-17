#ifndef UI_HH
#define UI_HH

#include "RealEstateAgency.hh"

class UI {
private:
  RealEstateAgency _controller;
  std::string _filePath;

  void _handleCommand();
  
public:
  void mainLoop();
};

#endif