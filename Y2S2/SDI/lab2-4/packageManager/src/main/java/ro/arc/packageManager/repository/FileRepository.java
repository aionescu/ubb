package ro.arc.packageManager.repository;

import java.io.File;
import java.io.IOException;
import java.lang.StackWalker.Option;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import ro.arc.packageManager.domain.BaseEntity;
import ro.arc.packageManager.domain.exceptions.AppException;
import ro.arc.packageManager.domain.exceptions.Contract;
import ro.arc.packageManager.domain.validators.Validator;
import ro.arc.packageManager.domain.validators.ValidatorException;

public class FileRepository<T extends BaseEntity<Long>> implements Repository<Long, T> {
  private Path path;
  private Validator<T> validator;
  private Class<T> cls;

  public FileRepository(String path, Validator<T> validator, Class<T> cls) {
    Contract.notNull(path, "path");
    Contract.notNull(validator, "validator");

    this.path = Path.of(path).toAbsolutePath();
    this.validator = validator;
    this.cls = cls;
  }

  private List<T> loadFromFile() {
    try {
      return Files.readAllLines(path).stream().map(r -> BaseEntity.ofCSVRow(r, cls)).collect(Collectors.toList());
    } catch (IOException e) {
      e.printStackTrace();
      throw new AppException("IO error: " + e.getMessage());
    }
  }

  private void writeToFile(List<T> l) {
    var text = l.stream().map(e -> e.toCSVRow()).collect(Collectors.joining("\n"));

    try {
      Files.write(path, text.getBytes());
    } catch (IOException e) {
      e.printStackTrace();
      throw new AppException("IO error: " + e.getMessage());
    }
  }

  @Override
  public Optional<T> findOne(Long id) {
    return loadFromFile().stream().filter(e -> e.getID().equals(id)).findFirst();
  }

  @Override
  public Iterable<T> findAll() {
    return loadFromFile().stream()::iterator;
  }

  @Override
  public Optional<T> save(T entity) throws ValidatorException {
    var entities = loadFromFile();

    var nextID = entities.stream().mapToLong(e -> e.getID()).max().orElse(-1) + 1;

    entity.setID(nextID);
    validator.validate(entity);

    entities.add(entity);
    writeToFile(entities);

    return Optional.empty();
  }

  @Override
  public Optional<T> delete(Long id) {
    var entities = loadFromFile();
    var existing = findOne(id);
    var filtered = entities.stream().filter(e -> !e.getID().equals(id)).collect(Collectors.toList());

    writeToFile(filtered);
    return existing;
  }

  @Override
  public Optional<T> update(T entity) throws ValidatorException {
    var entities = loadFromFile();
    var existing = findOne(entity.getID());
    var updated = entities.stream().map(e -> e.getID().equals(entity.getID()) ? entity : e).collect(Collectors.toList());

    writeToFile(updated);
    return existing;
  }
}
