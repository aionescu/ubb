#ifndef UI_HH
#define UI_HH

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include "Domain.hh"
#include "Vector.hh"
#include "Services.hh"

inline Vector<std::string> splitString(std::string string, char delimiter) {
  Vector<std::string> vector;

  std::stringstream stream{string};
  std::string token;

  while (std::getline(stream, token, delimiter)) {
    // Trim beginning.
    token.erase(token.begin(), std::find_if(token.begin(), token.end(), [](int ch) {
      return !std::isspace(ch);
    }));

    // Trim end.
    token.erase(std::find_if(token.rbegin(), token.rend(), [](int ch) {
      return !std::isspace(ch);
    }).base(), token.end());

    vector.append(token);
  }

  return vector;
}

inline Task taskFromParts(Vector<std::string> parts) {
  std::string title = parts[0];
  std::string type = parts[1];
  std::string lastPerformed = parts[2];
  int timesPerformed = std::stoi(parts[3]);
  std::string vision = parts[4];

  return Task{title, type, lastPerformed, timesPerformed, vision};
}

inline std::ostream& operator <<(std::ostream& stream, const Task& task) {
  stream
    << task.title() << ", "
    << task.type() << ", "
    << task.lastPerformed() << ", "
    << task.timesPerformed() << ", "
    << task.vision();

  return stream;
}

class UI {
  Services _services;
  std::map<std::string, void (UI::*)(std::stringstream&)> _commands;
public:
  UI() : _services{}, _commands{{
    { "help", &UI::help },
    { "exit", &UI::exit },
    { "mode", &UI::mode },
    { "add", &UI::add },
    { "update", &UI::update },
    { "delete", &UI::remove },
    { "list", &UI::list }
  }} {}

  void help(std::stringstream& args) {
    std::cout << "Available commands:\n";

    for (auto command : _commands) {
      std::cout << command.first << '\n';
    }
  }

  void exit(std::stringstream& args) {
    std::exit(0);
  }

  void mode(std::stringstream& args) {
    std::string mode;
    args >> mode;

    _services.setMode(mode);
  }

  void add(std::stringstream& args) {
    std::string buffer;
    std::getline(args, buffer);

    auto parts = splitString(buffer, ',');
    auto newTask = taskFromParts(parts);

    if (!_services.add(newTask))
      std::cout << "Error: Invalid command.\n";
  }

  void update(std::stringstream& args) {
    std::string buffer;
    std::getline(args, buffer);

    auto parts = splitString(buffer, ',');
    auto task = taskFromParts(parts);

    if (!_services.update(task))
      std::cout << "Error: Invalid command.\n";
  }

  void remove(std::stringstream& args) {
    std::string title;
    args >> title;
    
    if (!_services.remove(title))
      std::cout << "Error: Invalid command.\n";
  }

  void list(std::stringstream& args) {
    auto data = _services.data();

    for (int i = 0; i < data.length(); ++i)
      std::cout << data[i] << '\n';
  }

  void handleCommand() {
    std::string inputBuffer;
    std::getline(std::cin, inputBuffer);
    std::stringstream inputStream{inputBuffer};

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
    while (true) {
      std::cout << "> ";
      handleCommand();
    }
  }
};

#endif
