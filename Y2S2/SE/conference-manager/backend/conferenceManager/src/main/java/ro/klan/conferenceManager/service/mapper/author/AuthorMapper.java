package ro.klan.conferenceManager.service.mapper.author;

import ro.klan.conferenceManager.entity.Author;
import ro.klan.conferenceManager.service.author.AuthorDto;

public interface AuthorMapper {
  AuthorDto toService(Author entity);
  Author toEntity(AuthorDto dto);
}
