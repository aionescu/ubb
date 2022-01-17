package ro.arc.packageManager.core.repository;

import ro.arc.packageManager.core.domain.Maintainer;

import java.util.List;

public interface MaintainerRepository extends Repository<Maintainer, Long> {

    List<Maintainer> findAllByEmailContains(String input);
    List<Maintainer> findAllByUserNameContains(String input);
    List<Maintainer> findAllByFullNameContains(String input);
    boolean existsAllByUserNameOrEmail(String userName, String email);
    boolean existsAllByUserNameAndIdIsNot(String userName, Long id);
    boolean existsAllByEmailAndIdIsNot(String email, Long id);
}
