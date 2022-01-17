#ifndef SERVICES_HH
#define SERVICES_HH

#include <vector>
#include "Domain.hh"

class Services {
  std::vector<Illness> _illnesses;

public:
  void loadFromFile(std::string filePath);

  void add(Illness illness);
  std::vector<Illness> data() const;

  // Function that filters illnesses by the specified criteria, only returning
  // illnesses whose name or category contains the specified string.
  // If the criteria is empty, all illnesses are returned.
  // Input: criteria - String
  // Output: A vector containing only illnesses that match the specified criteria.
  // Throws: -
  std::vector<Illness> illnessesByCategoryOrName(std::string criteria) const;

  Illness getByName(std::string name) const;
};

#endif