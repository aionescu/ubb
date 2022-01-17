#include <fstream>
#include "RealEstateAgency.hh"

Dwelling RealEstateAgency::addDwelling(std::string type, double price, bool isProfitable) {
  Dwelling dwelling{type, price, isProfitable};
  _dwellings.push_back(dwelling);

  return dwelling;
}

void RealEstateAgency::addClient(std::string name, double salary, double* moneyFromInvestments) {
  std::shared_ptr<Client> client;

  if (moneyFromInvestments)
    client = std::make_shared<WealthyClient>(name, salary, *moneyFromInvestments);
  else
    client = std::make_shared<NormalClient>(name, salary);

  _clients.push_back(client);
}

std::vector<Dwelling> RealEstateAgency::getAllDwellings() {
  return _dwellings;
}

std::vector<std::shared_ptr<Client>> RealEstateAgency::getAllClients() {
  return _clients;
}

std::vector<std::shared_ptr<Client>> RealEstateAgency::getInterestedClients(std::string dwellingType) {
  std::vector<std::shared_ptr<Client>> clients;

  for (auto client : _clients)
    for (auto dwelling : _dwellings)
      if (dwelling.type() == dwellingType && client->isInterested(dwelling))
        clients.push_back(client);

  return clients;
}

void RealEstateAgency::writeToFile(std::string filePath) {
  std::ofstream outFile{filePath};

  for (auto client : _clients)
    outFile << client->toString() << ',' << client->totalIncome() << '\n';
}