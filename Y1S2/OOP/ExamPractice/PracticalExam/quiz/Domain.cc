#include "Domain.hh"

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