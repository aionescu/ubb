#include "UI.hh"

void UI::_tryPerform(bool result) const {
  if (!result)
    std::cout << "Error: Invalid arguments.\n";
}

UI::UI()
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
  }}
  { }

void UI::help(std::istringstream&) {
  std::cout << "Available commands:\n";
  
  for (auto command: _commands) {
    std::cout << command.first << '\n';
  }
}

void UI::exit(std::istringstream&) {
  _keepGoing = false;
}

void UI::mode(std::istringstream& commandArguments) {
  std::string mode;
  commandArguments >> mode;

  _services.setMode(mode);
}

void UI::fileLocation(std::istringstream& commandArguments) {
  std::string filePath;
  std::getline(commandArguments, filePath);

  trimString(filePath);
  _services.setFilePath(filePath);
}

void UI::add(std::istringstream& commandArguments) {
  std::string untokenizedLine;
  std::getline(commandArguments, untokenizedLine);

  auto tokenizedLine = splitString(untokenizedLine, ',');
  auto newTask = taskOfParts(tokenizedLine);

  _tryPerform(_services.add(newTask));
}

void UI::update(std::istringstream& commandArguments) {
  std::string untokenizedLine;
  std::getline(commandArguments, untokenizedLine);

  auto tokenizedLine = splitString(untokenizedLine, ',');
  auto task = taskOfParts(tokenizedLine);

  _tryPerform(_services.update(task));
}

void UI::remove(std::istringstream& commandArguments) {
  std::string title;
  commandArguments >> title;

  _tryPerform(_services.remove(title));
}

void UI::list(std::istringstream& commandArguments) {
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

void UI::next(std::istringstream&) {
  auto task = _services.next();

  if (task.first)
    std::cout << task.second << '\n';
}

void UI::save(std::istringstream& commandArguments) {
  std::string title;
  commandArguments >> title;

  _tryPerform(_services.save(title));
}

void UI::myList(std::istringstream&) {
  for (auto task : _services.servantTasks())
    std::cout << task << '\n';
}

void UI::handleCommand() {
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

void UI::mainLoop() {
  while (_keepGoing) {
    std::cout << "> ";
    handleCommand();
  }
}