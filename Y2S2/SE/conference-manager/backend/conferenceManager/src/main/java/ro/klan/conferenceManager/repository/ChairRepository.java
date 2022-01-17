package ro.klan.conferenceManager.repository;

import ro.klan.conferenceManager.entity.Chair;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ChairRepository extends JpaRepository<Chair, Long> {
    Optional<Chair> findByName(String name);
    Optional<Chair> findByEmail(String email);
}
