#ifndef SERVICES_HH
#define SERVICES_HH

#include "Repo.hh"
#include "Observer.hh"

template <typename TRepo>
class Services {
  TRepo _repo;

public:
  virtual ~Services() = default;
  
  template <typename TEntity>
  void loadFromFile(std::string path) {
    _repo.Repo<TEntity>::loadFromFile(path);
  }

  template <typename TEntity>
  void saveToFile(std::string path) {
    _repo.Repo<TEntity>::saveToFile(path);
  }

  template <typename TEntity>
  void add(TEntity entity) {
    _repo.Repo<TEntity>::add(entity);
  }

  template <typename TEntity>
  void update(TEntity oldEntity, TEntity newEntity) {
    _repo.Repo<TEntity>::update(oldEntity, newEntity);
  }

  template <typename TEntity>
  void remove(TEntity entity) {
    _repo.Repo<TEntity>::remove(entity);
  }
  
  template <typename TEntity>
  TEntity& getByPredicate(std::function<bool(const TEntity&)> predicate) {
    return _repo.Repo<TEntity>::getByPredicate(predicate);
  }

  template <typename TEntity>
  std::vector<TEntity>& getAllData() {
    return _repo.Repo<TEntity>::getAllData();
  }
};

class InvalidStar : public std::exception { };

class Service : public Services<Repository>, public Observable {
public:
  void sortByConstellation();

  // Member function that attempts to add the star with the specified components to the repository.
  // Input: name - The star's name, constellation - The consetllation to which the star belongs,
  //        rightAscension - The star's right ascension coordinate, declination - The star's decalation,
  //        diameter - The star's diameter.
  // Output: -
  // Throws: InvalidStar if the star's name is empty, if the star's diameter is negative, or if the star's name is not unique.
  void tryAddStar(std::string name, std::string constellation, int rightAscension, int declination, int diameter);

  std::vector<Star> starsByConstellation(std::string constellation);
};

#endif
