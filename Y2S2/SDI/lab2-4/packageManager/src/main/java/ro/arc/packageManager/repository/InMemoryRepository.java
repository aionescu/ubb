package ro.arc.packageManager.repository;

import ro.arc.packageManager.domain.BaseEntity;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.Validator;
import ro.arc.packageManager.domain.validators.ValidatorException;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class InMemoryRepository<T extends BaseEntity<Long>> implements Repository<Long, T> {
  private Map<Long, T> entities;
  private Validator<T> validator;
  private Long nextID;

  public InMemoryRepository(Validator<T> validator) {
    this.validator = validator;
    entities = new HashMap<>();
    nextID = 0l;
  }

  @Override
  public Optional<T> findOne(Long id) {
    Contract.notNull(id, "id");

    return Optional.ofNullable(entities.get(id));
  }

  @Override
  public Iterable<T> findAll() {
    return entities.entrySet().stream().map(entry -> entry.getValue()).collect(Collectors.toSet());
  }

  @Override
  public Optional<T> save(T entity) throws ValidatorException {
    Contract.notNull(entity, "entity");

    entity.setID(nextID);
    validator.validate(entity);
    ++nextID;

    return Optional.ofNullable(entities.putIfAbsent(entity.getID(), entity));
  }

  @Override
  public Optional<T> delete(Long id) {
    Contract.notNull(id, "id");

    return Optional.ofNullable(entities.remove(id));
  }

  @Override
  public Optional<T> update(T entity) throws ValidatorException {
    Contract.notNull(entity, "entity");

    validator.validate(entity);
    return Optional.ofNullable(entities.computeIfPresent(entity.getID(), (k, v) -> entity));
  }
}
