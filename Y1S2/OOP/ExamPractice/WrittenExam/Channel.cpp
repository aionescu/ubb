#include <cstdlib>
#include <ctime>
#include <exception>
#include <iostream>
#include <memory>
#include <string>

class Channel {
public:
  virtual ~Channel() { }
  virtual void send(std::string message) = 0;
};

class Failover : public Channel {
  std::unique_ptr<Channel> _chA, _chB;

public:
  Failover(std::unique_ptr<Channel> chA, std::unique_ptr<Channel> chB)
    : _chA{std::move(chA)}, _chB{std::move(chB)}
  { }

  void send(std::string message) override {
    try {
      _chA->send(message);
    } catch (...) {
      _chB->send(message);
    }
  }
};

class Contact {
  std::unique_ptr<Channel> _chA, _chB, _chC;

public:
  Contact(std::unique_ptr<Channel> chA, std::unique_ptr<Channel> chB, std::unique_ptr<Channel> chC)
    : _chA{std::move(chA)}, _chB{std::move(chB)}, _chC{std::move(chC)}
  { }

  void sendAlarm(std::string message) {
    auto done = false;

    while (!done)
      try {
        _chA->send(message);
        done = true;
      } catch (...) {
        try {
          _chB->send(message);
          done = true;
        } catch (...) {
          try {
            _chC->send(message);
            done = true;
          } catch (...) { }
        }
      }
  }
};

class LineIsBusy : public std::exception { };

class Telephone : public Channel {
  std::string _number;

public:
  Telephone(std::string number) : _number{number} { }

  void send(std::string message) override {
    std::cout << "Dialing " << _number << ".\n";

    if (rand() % 10 == 0)
      throw LineIsBusy{};
  }
};

class Fax : public Telephone {
public:
  Fax(std::string number) : Telephone{number} { }

  void send(std::string message) override {
    Telephone::send(message);
    std::cout << "Sending Fax.\n";
  }
};

class SMS : public Telephone {
public:
  SMS(std::string number) : Telephone{number} { }

  void send(std::string message) override {
    Telephone::send(message);
    std::cout << "Sending SMS.\n";
  }
};

int main() {
  std::srand(std::time(0));
  
  Contact contact{
    std::make_unique<Telephone>("000-000"),
    std::make_unique<Failover>(
      std::make_unique<Fax>("111-111"),
      std::make_unique<SMS>("111-111")),
    std::make_unique<Failover>(
      std::make_unique<Telephone>("222-222"),
      std::make_unique<Failover>(
        std::make_unique<Fax>("222-222"),
        std::make_unique<SMS>("222-222")))};

  contact.sendAlarm("Hello?");
}