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

  void _tryPerform(bool result) const {
    if (!result)
      std::cout << "Error: Invalid arguments.\n";
  }

public:
  UI()
  : _keepGoing{true}, _services{}, _commands{{
    { "help", &UI::help },
    { "exit", &UI::exit },
    { "mode", &UI::mode },
    { "fileLocation", &UI::fileLocation },
    { "add", &UI::add },
    { "update", &UI::update },
    { "delete", &UI::remove },
    { "list", &UI::list },
    { "next", &UI::next },
    { "save", &UI::save },
    { "mylist", &UI::myList }
  }} {}

  void help(std::istringstream&) {
    std::cout << "Available commands:\n";
    
    for (auto command: _commands) {
      std::cout << command.first << '\n';
    }
  }

  void exit(std::istringstream&) {
    _keepGoing = false;
  }

  void mode(std::istringstream& commandArguments) {
    std::string mode;
    commandArguments >> mode;

    _services.setMode(mode);
  }

  void fileLocation(std::istringstream& commandArguments) {
    std::string filePath;
    std::getline(commandArguments, filePath);

    trimString(filePath);
    _services.setFilePath(filePath);
  }

  void add(std::istringstream& commandArguments) {
    std::string untokenizedLine;
    std::getline(commandArguments, untokenizedLine);

    auto tokenizedLine = splitString(untokenizedLine, ',');
    auto newTask = taskOfParts(tokenizedLine);

    _tryPerform(_services.add(newTask));
  }

  void update(std::istringstream& commandArguments) {
    std::string untokenizedLine;
    std::getline(commandArguments, untokenizedLine);

    auto tokenizedLine = splitString(untokenizedLine, ',');
    auto task = taskOfParts(tokenizedLine);

    _tryPerform(_services.update(task));
  }

  void remove(std::istringstream& commandArguments) {
    std::string title;
    commandArguments >> title;
  
    _tryPerform(_services.remove(title));
  }

  void list(std::istringstream& commandArguments) {
    std::string untokenizedLine;
    std::getline(commandArguments, untokenizedLine);

    auto tokenizedLine = splitString(untokenizedLine, ',');

    std::vector<Task> tasks;

    if (tokenizedLine.size() == 0 || tokenizedLine.at(0).empty())
      tasks = _services.allTasks();
    else
      tasks = _services.tasksByTimesPerformed(tokenizedLine.at(0), std::stoi(tokenizedLine.at(1)));

    std::cout << tasks;
  }

  void next(std::istringstream&) {
    auto task = _services.next();

    if (task.first)
      std::cout << task.second << '\n';
  }

  void save(std::istringstream& commandArguments) {
    std::string title;
    commandArguments >> title;

    _tryPerform(_services.save(title));
  }

  void myList(std::istringstream&) {
    for (auto task : _services.servantTasks())
      std::cout << task << '\n';
  }

  void handleCommand() {
    std::string inputBuffer;
    std::getline(std::cin, inputBuffer);
    std::istringstream inputStream{inputBuffer};

    std::string commandString;
    inputStream >> commandString;

    auto command = _commands.find(commandString);

    if (command == _commands.end()) {
      std::cout << "Command not recognized.\n";
      return;
    }
    
    try {
      auto commandFunction = command->second;
      (this->*commandFunction)(inputStream);
    } catch (WrongModeException& e) {
      std::cout << "Error: Wrong mode.\n";
    } catch (...) {
      std::cout << "Error: Wrong arguments.\n";
    }
  }

  void mainLoop() {
    while (_keepGoing) {
      std::cout << "> ";
      handleCommand();
    }
  }
};

#endif