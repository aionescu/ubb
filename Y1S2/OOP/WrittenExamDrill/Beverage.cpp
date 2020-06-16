#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>

class Beverage {
  std::string _description;

public:
  virtual ~Beverage() { }

  Beverage(std::string description = "") : _description{description} { }

  std::string description() const noexcept {
    return _description;
  }

  virtual double price() const noexcept = 0;

  virtual void print() const {
    std::cout << _description << ", worth " << price() << " RON.\n";
  }
};

class Coffee : public Beverage {
public:
  Coffee() : Beverage{"Coffee"} { }

  double price() const noexcept override {
    return 2.5;
  }
};

class Tea : public Beverage {
public:
  Tea() : Beverage{"Tea"} { }

  double price() const noexcept override {
    return 1.5;
  }
};

class BeverageWithMilk : public Beverage {
  std::unique_ptr<Beverage> _beverage;
  int _milkCount;

public:
  BeverageWithMilk(std::unique_ptr<Beverage> beverage, int milkCount)
    : _beverage{std::move(beverage)}
    , _milkCount{milkCount}
  { }

  double price() const noexcept override {
    return _beverage->price() + 0.5 * _milkCount;
  }

  void print() const override {
    std::cout
      << _beverage->description() << " with "
      << _milkCount << " portions of milk, worth "
      << price() << " RON.\n";
  }
};

class BeverageMachine {
public:
  std::unique_ptr<Beverage> prepare(std::string beverageType, int milkCount) {
    std::unique_ptr<Beverage> beverage;

    if (beverageType == "Coffee")
      beverage = std::make_unique<Coffee>();
    else if (beverageType == "Tea")
      beverage = std::make_unique<Tea>();
    else
      throw std::runtime_error{"Beverage type not supported."};

    if (milkCount > 0)
      beverage = std::make_unique<BeverageWithMilk>(std::move(beverage), milkCount);

    beverage->print();
    return beverage;
  }
};

int main() {
  BeverageMachine machine;

  machine.prepare("Tea", 0);
  machine.prepare("Coffee", 1);
  machine.prepare("Coffee", 2);
}