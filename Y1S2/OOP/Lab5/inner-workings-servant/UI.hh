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

// Class that represents the state of the app's user interface.
// The convention for commands is that each command is a separate
// member function that returns void and  takes a `stringstream&`
// as argument. The stringstream is used to read the command's
// command-line arguments.
class UI {
  bool _interactiveMode;
  std::istream& _inStream;
  std::ostream& _outStream;

  Services _services;
  std::map<std::string, void (UI::*)(std::istringstream&)> _commands;

  void _tryPerform(bool result) const {
    if (!result)
      _outStream << "Error: Invalid arguments.\n";
  }

public:
  UI(std::istream& inStream, std::ostream& outStream, bool interactiveMode = true)
  : _interactiveMode{interactiveMode},
    _inStream{inStream},
    _outStream{outStream},
    _services{}, _commands{{
    { "help", &UI::help },
    { "exit", &UI::exit },
    { "mode", &UI::mode },
    { "add", &UI::add },
    { "update", &UI::update },
    { "delete", &UI::remove },
    { "list", &UI::list },
    { "next", &UI::next },
    { "save", &UI::save },
    { "mylist", &UI::myList }
  }} {}

  void help(std::istringstream&) {
    _outStream << "Available commands:\n";

    for (auto command : _commands) {
      _outStream << command.first << '\n';
    }
  }

  void exit(std::istringstream&) {
    std::exit(0);
  }

  void mode(std::istringstream& args) {
    std::string mode;
    args >> mode;

    _services.setMode(mode);
  }

  void add(std::istringstream& args) {
    std::string buffer;
    std::getline(args, buffer);

    auto parts = splitString(buffer, ',');
    auto newTask = taskFromParts(parts);

    _tryPerform(_services.add(newTask));
  }

  void update(std::istringstream& args) {
    std::string buffer;
    std::getline(args, buffer);

    auto parts = splitString(buffer, ',');
    auto task = taskFromParts(parts);

    _tryPerform(_services.update(task));
  }

  void remove(std::istringstream& args) {
    std::string title;
    args >> title;
  

    _tryPerform(_services.remove(title));
  }

  void list(std::istringstream& args) {
    std::string buffer;
    std::getline(args, buffer);

    auto parts = splitString(buffer, ',');

    Vector<Task> tasks;

    if (parts.length() == 0 || parts[0].empty())
      tasks = _services.allTasks();
    else
      tasks = _services.tasksByTimesPerformed(parts[0], std::stoi(parts[1]));

    for (auto task : tasks)
      _outStream << task << '\n';
  }

  void next(std::istringstream&) {
    auto task = _services.next();

    if (task.first)
      _outStream << task.second << '\n';
  }

  void save(std::istringstream& args) {
    std::string title;
    args >> title;

    _tryPerform(_services.save(title));
  }

  void myList(std::istringstream&) {
    for (auto task : _services.servantTasks())
      _outStream << task << '\n';
  }

  void handleCommand() {
    std::string inputBuffer;
    std::getline(_inStream, inputBuffer);
    std::istringstream inputStream{inputBuffer};

    std::string commandString;
    inputStream >> commandString;

    auto command = _commands.find(commandString);

    if (command == _commands.end()) {
      _outStream << "Command not recognized.\n";
      return;
    }
    
    try {
      auto commandFunction = command->second;
      (this->*commandFunction)(inputStream);
    } catch (WrongModeException& e) {
      _outStream << "Error: Wrong mode.\n";
    } catch (...) {
      _outStream << "Error: Wrong arguments.\n";
    }
  }

  void mainLoop() {
    while (true) {
      if (_interactiveMode)
        _outStream << "> ";

      handleCommand();
    }
  }
};

#endif