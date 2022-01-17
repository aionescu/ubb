#ifndef REAL_ESTATE_AGENCY_HH
#define REAL_ESTATE_AGENCY_HH

#include <iostream>
#include <memory>
#include <vector>
#include "Domain.hh"

class RealEstateAgency {
private:
  std::vector<Dwelling> _dwellings;
  std::vector<std::shared_ptr<Client>> _clients;

public:
  Dwelling addDwelling(std::string type, double price, bool isProfitable);
  void addClient(std::string name, double salary, double* moneyFromInvestments);

  std::vector<Dwelling> getAllDwellings();
  std::vector<std::shared_ptr<Client>> getAllClients();
  std::vector<std::shared_ptr<Client>> getInterestedClients(std::string dwellingType);

  void writeToFile(std::string filePath);
};

#endif