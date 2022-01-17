#include <algorithm>
#include "Observer.hh"

Observer::~Observer() { }

Observable::~Observable() { }

void Observable::addObserver(Observer* observer) {
  _observers.push_back(observer);
}

void Observable::removeObserver(Observer* observer) {
  auto it = std::find(_observers.begin(), _observers.end(), observer);

  if (it != _observers.end())
    _observers.erase(it);
}

void Observable::notify() {
  for (auto observer : _observers) 
    observer->update();
}