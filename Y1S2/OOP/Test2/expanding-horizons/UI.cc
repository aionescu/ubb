#include <algorithm>
#include <iostream>
#include <sstream>
#include "UI.hh"

void trimString(std::string& string) {
  auto isNotSpace = [](char character) { return !std::isspace(character); };
  
  // Trim beginning.
  string.erase(string.begin(), std::find_if(string.begin(), string.end(), isNotSpace));

  // Trim end.
  string.erase(std::find_if(string.rbegin(), string.rend(), isNotSpace).base(), string.end());
}

std::vector<std::string> splitString(const std::string& string, char delimiter) {
  std::vector<std::string> vector;

  std::istringstream stream{string};
  std::string token;
  
  while (std::getline(stream, token, delimiter)) {
    trimString(token);
    vector.push_back(token);
  }

  return vector;
}

void UI::_handleCommand() {
  std::string command;
  std::cin >> command;

  std::string argumentsBuffer;
  std::getline(std::cin, argumentsBuffer);
  auto arguments = splitString(argumentsBuffer, ',');

  if (command == "exit") {
    std::exit(0);
  } else if (command == "addClient") {
    auto name = arguments.at(0);
    auto salary = std::stod(arguments.at(1));
    
    if (arguments.size() == 2)
      _controller.addClient(name, salary, nullptr);
    else {
      auto moneyFromInvestments = std::stod(arguments.at(2));
      _controller.addClient(name, salary, &moneyFromInvestments);
    }
  } else if (command == "addDwelling") {
    auto type = arguments.at(0);
    auto price = std::stod(arguments.at(1));
    auto isProfitable = arguments.at(2) == "true";

    _controller.addDwelling(type, price, isProfitable);
  } else if (command == "fileLocation") {
    _filePath = arguments.at(0);
  } else if (command == "list") {
    if (_filePath != "")
      _controller.writeToFile(_filePath);

    if (arguments.empty()) {
      auto clients = _controller.getAllClients();

      for (auto client : clients)
        std::cout << client->toString() << '\n';

      for (auto dwelling : _controller.getAllDwellings())
        std::cout << dwelling.toString() << '\n';
    } else {
      auto clients = _controller.getInterestedClients(arguments.at(0));

      for (auto client : clients)
        std::cout << client->toString() << '\n';
    }
  } else {
    std::cout << "Command not recognized.\n";
  }
}

void UI::mainLoop() {
  while (true) {
    std::cout << "> ";
    _handleCommand();
  }
}