#ifndef SERVICES_HH
#define SERVICES_HH

#include "Repo.hh"

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
  std::vector<TEntity> getAllData() {
    return _repo.Repo<TEntity>::getAllData();
  }
};

#endif