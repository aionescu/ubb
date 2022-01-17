package ro.arc.packageManager.core.repository;

import ro.arc.packageManager.core.domain.Package;

import java.util.List;
import java.util.Optional;

public interface PackageRepository extends Repository<Package, Long> {

    List<Package> findAllByDescriptionContains(String input);
    List<Package> findAllBySourceRepoContains(String input);
    List<Package> findAllByLicenseContains(String input);
    List<Package> findAllByNameContains(String input);

    boolean existsAllByName(String name);
    boolean existsAllByNameAndIdIsNot(String name, Long id);
    Optional<Package> findByName(String name);
}
