package ro.klan.conferenceManager.repository;

import ro.klan.conferenceManager.entity.Paper;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface PaperRepository extends JpaRepository<Paper, Long> {
    Optional<Paper> findByName(String name);
}
