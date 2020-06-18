#include <iostream>
#include <memory>
#include <string>
#include <vector>

class Action {
public:
  virtual ~Action() { }
  virtual void execute() = 0;
};

class CreateAction : public Action {
public:
  void execute() override {
    std::cout << "Create file.\n";
  }
};

class ExitAction : public Action {
public:
  void execute() override {
    std::cout << "Exit application.\n";
  }
};

class MenuItem {
  std::string _text;
  std::shared_ptr<Action> _action;

public:
  MenuItem(std::string text = "", std::shared_ptr<Action> action = {})
    : _text{text}
    , _action{action}
  { }

  virtual ~MenuItem() { }

  virtual void print(int indentation = 0) {
    std::cout << std::string(indentation, ' ') << _text << '\n';
  }

  void clicked() {
    if (_action)
      _action->execute();
  }
};

class Menu : public MenuItem, public std::vector<std::shared_ptr<MenuItem>> {
public:
  Menu(std::string text) : MenuItem{text} { }

  void add(std::shared_ptr<MenuItem> item) {
    push_back(item);
  }

  void print(int indentation = 0) override {
    MenuItem::print(indentation);

    for (auto& item : *this)
      item->print(indentation + 2);
  }
};

class MenuBar : public std::vector<std::shared_ptr<Menu>> {
public:
  void add(std::shared_ptr<Menu> menu) {
    push_back(menu);
  }

  void print() {
    for (auto& menu : *this)
      menu->print();
  }
};

int main() {
  auto exit = std::make_shared<MenuItem>("Exit", std::make_shared<ExitAction>());
  auto text = std::make_shared<MenuItem>("Text", std::make_shared<CreateAction>());
  auto cpp = std::make_shared<MenuItem>("C++", std::make_shared<CreateAction>());

  auto new_ = std::make_shared<Menu>("New");
  new_->add(text);
  new_->add(cpp);

  auto file = std::make_shared<Menu>("File");
  file->add(new_);
  file->add(exit);

  auto about = std::make_shared<Menu>("About");

  MenuBar menuBar;
  menuBar.add(file);
  menuBar.add(about);

  menuBar.print();

  std::cout << '\n';

  file->clicked();
  new_->clicked();
  cpp->clicked();
  exit->clicked();
}