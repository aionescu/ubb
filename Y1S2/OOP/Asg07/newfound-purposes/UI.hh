#ifndef UI_HH
#define UI_HH

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>
#include "Domain.hh"
#include "Services.hh"

// Class that represents the state of the app's user interface.
// The convention for commands is that each command is a separate
// member function that returns void and  takes a `istringstream&`
// as argument. The istringstream is used to read the command's
// command-line arguments.
class UI {
  bool _keepGoing;
  Services _services;
  std::map<std::string, void (UI::*)(std::istringstream&)> _commands;

  void _tryPerform(bool result) const;

public:
  UI();

  void help(std::istringstream&);
  void exit(std::istringstream&);

  void mode(std::istringstream& commandArguments);
  void fileLocation(std::istringstream& commandArguments);

  void add(std::istringstream& commandArguments);
  void update(std::istringstream& commandArguments);
  void remove(std::istringstream& commandArguments);
  void list(std::istringstream& commandArguments);

  void next(std::istringstream&);
  void save(std::istringstream& commandArguments);
  void myList(std::istringstream&);

  void handleCommand();
  void mainLoop();
};

#endif