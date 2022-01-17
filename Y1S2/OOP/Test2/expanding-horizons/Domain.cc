#include "Domain.hh"

Dwelling::Dwelling(std::string type, double price, bool isProfitable)
  : _type{type}, _price{price}, _isProfitable{isProfitable}
  { }

std::string Dwelling::type() {
  return _type;
}

bool Dwelling::isProfitable() {
  return _isProfitable;
}

double Dwelling::normalBankRate() {
  return _price / 1000;
}

double Dwelling::largeBankRate() {
  return _price / 100;
}

std::string Dwelling::toString() {
  return _type + "," + std::to_string(_price) + "," + (_isProfitable ? "true" : "false");
}

Client::Client(std::string name, double salary)
  : _name{name}, _salary{salary}
  { }

double Client::totalIncome() {
  return _salary;
}

std::string Client::toString() {
  return _name + "," + std::to_string(_salary);
}

NormalClient::NormalClient(std::string name, double salary)
  : Client{name, salary}
  { }

bool NormalClient::isInterested(Dwelling dwelling) {
  return dwelling.normalBankRate() <= totalIncome();
}

WealthyClient::WealthyClient(std::string name, double salary, double moneyFromInvestments)
  : Client{name, salary}, _moneyFromInvestments{moneyFromInvestments}
  { }

double WealthyClient::totalIncome() {
  return _salary + _moneyFromInvestments;
}

std::string WealthyClient::toString() {
  return Client::toString() + "," + std::to_string(_moneyFromInvestments);
}

bool WealthyClient::isInterested(Dwelling dwelling) {
  return dwelling.largeBankRate() <= totalIncome() && dwelling.isProfitable();
}