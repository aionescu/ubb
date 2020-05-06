#ifndef DOMAIN_HH
#define DOMAIN_HH

#include <string>

class Dwelling {
private:
  std::string _type;
  double _price;
  bool _isProfitable;

public:
  Dwelling(std::string type, double price, bool isProfitable);

  std::string type();
  bool isProfitable();
  double normalBankRate();
  double largeBankRate();

  std::string toString();
};

class Client {
protected:
  std::string _name;
  double _salary;

  Client(std::string name, double salary);

public:
  virtual ~Client() { }
  virtual double totalIncome();
  virtual std::string toString();
  virtual bool isInterested(Dwelling dwelling) = 0;
};

class NormalClient : public Client {
public:
  NormalClient(std::string name, double salary);

  bool isInterested(Dwelling dwelling) override;
};

class WealthyClient : public Client {
private:
  double _moneyFromInvestments;

public:
  WealthyClient(std::string name, double salary, double moneyFromInvestments);

  double totalIncome() override;
  std::string toString() override;
  bool isInterested(Dwelling dwelling) override;
};

#endif