#include <algorithm>
#include <iostream>
#include <memory>
#include <vector>

class Expression {
public:
  virtual ~Expression() { }
  virtual double evaluate() const = 0;
};

class Constant : public Expression {
  double _value;

public:
  Constant(double value = 0) : _value{value} { }

  double evaluate() const override {
    return _value;
  }
};

class UnaryExpression : public Expression {
  std::unique_ptr<Expression> _expr;

public:
  UnaryExpression(std::unique_ptr<Expression> expr = {}) : _expr{std::move(expr)} { }

  double evaluate() const override {
    return _expr->evaluate();
  }
};

class Negate : public UnaryExpression {
public:
  Negate(std::unique_ptr<Expression> expr = {}) : UnaryExpression{std::move(expr)} { }

  double evaluate() const override {
    return -UnaryExpression::evaluate();
  }
};

class BinaryExpression : public UnaryExpression {
protected:
  std::unique_ptr<Expression> _left, _right;

public:
  BinaryExpression(std::unique_ptr<Expression> left = {}, std::unique_ptr<Expression> right = {})
    : _left{std::move(left)}, _right{std::move(right)}
  { }
};

class Adder : public BinaryExpression {
public:
  Adder(std::unique_ptr<Expression> left = {}, std::unique_ptr<Expression> right = {})
    : BinaryExpression{std::move(left), std::move(right)}
  { }

  double evaluate() const override {
    return _left->evaluate() + _right->evaluate();
  }
};

class Subtracter : public BinaryExpression {
public:
  Subtracter(std::unique_ptr<Expression> left = {}, std::unique_ptr<Expression> right = {})
    : BinaryExpression{std::move(left), std::move(right)}
  { }

  double evaluate() const override {
    return _left->evaluate() - _right->evaluate();
  }
};

class MathHomework {
  std::vector<std::unique_ptr<Expression>> _expressions;

public:
  void addExpression(std::unique_ptr<Expression> expr) {
    _expressions.push_back(std::move(expr));
  }

  std::vector<double> getResults() {
    std::vector<double> results;
    results.resize(_expressions.size());

    std::transform(_expressions.begin(), _expressions.end(), results.begin(),
      [&](const std::unique_ptr<Expression>& expr) { return expr->evaluate(); });

    return results;
  }
};

// -5 + (9 - 3) and -(4 + 2) - (-10)
int main() {
  MathHomework hw;

  hw.addExpression(
    std::make_unique<Adder>(
      std::make_unique<Constant>(-5),
      std::make_unique<Subtracter>(
        std::make_unique<Constant>(9),
        std::make_unique<Constant>(3))));

  hw.addExpression(
    std::make_unique<Subtracter>(
      std::make_unique<Negate>(
        std::make_unique<Adder>(
          std::make_unique<Constant>(4),
          std::make_unique<Constant>(2))),
        std::make_unique<Constant>(-10)));

  for (auto result : hw.getResults())
    std::cout << result << '\n';
}