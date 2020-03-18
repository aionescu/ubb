#ifndef UI_HH
#define UI_HH

#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include "Domain.hh"
#include "Vector.hh"
#include "Services.hh"

Vector<std::string> splitString(std::string string, char delimiter) {
  Vector<std::string> vector;

  std::stringstream stream(string);
  std::string token;

  while (std::getline(stream, token, delimiter)) {
    token.erase(token.begin(), std::find_if(token.begin(), token.end(), [](int ch) {
      return !std::isspace(ch);
    }));

    token.erase(std::find_if(token.rbegin(), token.rend(), [](int ch) {
      return !std::isspace(ch);
    }).base(), token.end());

    vector.append(token);
  }

  return vector;
}

Task taskFromParts(Vector<std::string> parts) {
  std::string title = parts[0];
  std::string type = parts[1];
  std::string lastPerformed = parts[2];
  int timesPerformed = std::stoi(parts[3]);
  std::string vision = parts[4];

  return Task{title, type, lastPerformed, timesPerformed, vision};
}

std::string taskToString(Task task) {
  std::stringstream stream;

  stream
    << task.title() << ", "
    << task.type() << ", "
    << task.lastPerformed() << ", "
    << task.timesPerformed() << ", "
    << task.vision();

  return stream.str();
}

class UI {
  Services _services;

public:
  void mode(std::string mode) {
    _services.setMode(mode);
  }

  void add(Task newTask) {
    if (!_services.add(newTask))
      std::cout << "Error: Invalid command.\n";
  }

  void update(Task task) {
    if (!_services.update(task))
      std::cout << "Error: Invalid command.\n";
  }

  void remove(std::string title) {
    if (!_services.remove(title))
      std::cout << "Error: Invalid command.\n";
  }

  void list() {
    auto data = _services.data();

    for (int i = 0; i < data.length(); ++i)
      std::cout << taskToString(data[i]) << '\n';
  }

  bool handleCommand() {
    std::string command;
    std::cin >> command;

    if (command.size() == 0) {
      std::cout << "Error: Command expected.\n";
      return true;
    }

    if (command == "exit")
      return false;
    else if (command == "list")
      list();
    else {
      std::string input;
      std::getline(std::cin, input);

      auto parts = splitString(input, ',');
      
      if (command == "mode")
        mode(parts[0]);
      else if (command == "add")
        add(taskFromParts(parts));
      else if (command == "update")
        update(taskFromParts(parts));
      else if (command == "delete")
        remove(parts[0]);
      else
        std::cout << "Command not recognized.\n";
    }

    return true;
  }

  void mainLoop() {
    bool keepGoing = true;

    do {
      std::cout << "> ";

      try {
        keepGoing = handleCommand();
      } catch (std::runtime_error& e) {
        std::cout << "Error: Wrong mode.\n";
      } catch (...) {
        std::cout << "Error: Wrong arguments.\n";
      }
    } while (keepGoing);
  }
};

#endif