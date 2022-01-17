package ro.klan.conferenceManager.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import ro.klan.conferenceManager.entity.Section;

import java.util.Optional;

public interface SectionRepository extends JpaRepository<Section, Long>
{
    Optional<Section> findByName(String name);
}
