#include "Services.hh"

void Service::sortByConstellation() {
  auto& data = this->getAllData<Star>();

  std::sort(data.begin(), data.end(),
    [&](const Star& a, const Star& b) {
      return a.get<Star::constellation>() < b.get<Star::constellation>();
    });
}

void Service::tryAddStar(std::string name, std::string constellation, int rightAscension, int declination, int diameter) {
  if (name.empty() || diameter <= 0)
    throw InvalidStar{};

  bool exists = true;

  try {
    this->getByPredicate<Star>([&](const Star& s) { return s.get<Star::name>() == name; });
  } catch (...) {
    exists = false;
  }

  if (exists)
    throw InvalidStar{};
  else {
    this->add<Star>(Star{name, constellation, rightAscension, declination, diameter});
    this->notify();
  }
}

std::vector<Star> Service::starsByConstellation(std::string constellation) {
  std::vector<Star> stars;

  for (auto star : this->getAllData<Star>())
    if (star.get<Star::constellation>() == constellation)
      stars.push_back(star);

  return stars;
}