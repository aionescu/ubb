package ro.klan.conferenceManager.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.klan.conferenceManager.entity.Conference;

import java.util.Optional;

public interface ConferenceRepository extends JpaRepository<Conference, Long> {
    Optional<Conference> findById(Long id);
}
