package ro.klan.conferenceManager.service.author;

import ro.klan.conferenceManager.repository.AuthorRepository;
import ro.klan.conferenceManager.service.mapper.author.AuthorMapper;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class AuthorServiceImpl implements AuthorService {
  private final AuthorRepository authorRepository;
  private final AuthorMapper authorMapper;
}
