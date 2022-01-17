#include <iostream>
#include <memory>
#include <vector>

class SaleItem {
  int _code;
  double _price;

public:
  SaleItem(int code, double price) : _code{code}, _price{price} { }

  int code() const noexcept {
    return _code;
  }

  double price() const noexcept {
    return _price;
  }
};

class Sale : public std::vector<SaleItem> { };

class Discount {
public:
  virtual ~Discount() { }
  virtual double calc(Sale sale) const noexcept = 0;
};

class ItemDiscount : public Discount {
  int _code;
  int _percentage; // The UML diagram says this is an `int`, though it would make more sense for it to be a `double`.

public:
  ItemDiscount(int code, int percentage) : _code{code}, _percentage{percentage} { }

  double calc(Sale sale) const noexcept override {
    double discount = 0;

    for (auto item : sale)
      if (item.code() == _code)
        discount += item.price() * (0.01 * _percentage);

    return discount;
  }
};

class SumDiscount : public Discount, public std::vector<std::unique_ptr<Discount>> {
public:
  void add(std::unique_ptr<Discount> discount) {
    push_back(std::move(discount));
  }

  double calc(Sale sale) const noexcept override {
    double total = 0;

    for (auto& discount : *this)
      total += discount->calc(sale);
    
    return total;
  }
};

int main() {
  Sale sale;
  sale.push_back({0, 100});
  sale.push_back({1, 150});
  sale.push_back({2, 200});

  auto discount = std::make_unique<SumDiscount>();
  discount->add(std::make_unique<ItemDiscount>(0, 10));
  discount->add(std::make_unique<ItemDiscount>(1, 15));

  std::cout << "Total discount: " << discount->calc(sale) << " RON.\n";
}