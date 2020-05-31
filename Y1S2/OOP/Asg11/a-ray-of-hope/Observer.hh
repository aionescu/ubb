#ifndef OBSERVER_HH
#define OBSERVER_HH

#include <vector>

class Observer {
public:
  virtual ~Observer();
  virtual void update() = 0;
};

class Observable {
private:
  std::vector<Observer*> _observers;

public:
  virtual ~Observable();

  void addObserver(Observer* observer);
  void removeObserver(Observer* observer);
  
  void notify();
};

#endif
