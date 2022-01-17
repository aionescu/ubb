#include <fstream>
#include <stdexcept>
#include "Services.hh"

void Services::loadFromFile(std::string filePath) {
  std::ifstream inFile{filePath};

  if (!inFile.good())
    return;

  inFile >> _illnesses;
}

void Services::add(Illness illness) {
  _illnesses.push_back(illness);
}

std::vector<Illness> Services::data() const {
  return _illnesses;
}

std::vector<Illness> Services::illnessesByCategoryOrName(std::string criteria) const {
  if (criteria.empty())
    return _illnesses;
    
  std::vector<Illness> result;

  for (auto illness : _illnesses)
    if (illness.category().find(criteria) != std::string::npos || illness.name().find(criteria) != std::string::npos)
      result.push_back(illness);

  return result;
}

Illness Services::getByName(std::string name) const {
  for (auto illness : _illnesses)
    if (illness.name() == name)
      return illness;

  throw std::runtime_error{"Illness not found."};
}