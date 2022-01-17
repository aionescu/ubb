package ro.klan.conferenceManager.service.mapper.author;

import ro.klan.conferenceManager.entity.Author;
import ro.klan.conferenceManager.service.author.AuthorDto;
import ro.klan.conferenceManager.service.mapper.paper.PaperMapper;

import java.util.stream.Collectors;

import org.springframework.stereotype.Service;

import lombok.AllArgsConstructor;

@Service
@AllArgsConstructor
public
class AuthorMapperImpl implements AuthorMapper {
  private final PaperMapper paperMapper;

  @Override
  public AuthorDto toService(Author entity) {
    var papers = entity.getPapers().stream().map(paperMapper::toService).collect(Collectors.toList());
    return new AuthorDto(entity.getId(), entity.getName(), entity.getAffiliation(), entity.getEmail(), papers);
  }

  @Override
  public Author toEntity(AuthorDto dto) {
    var papers = dto.getPapers().stream().map(paperMapper::toEntity).collect(Collectors.toSet());
    return new Author(dto.getId(), dto.getName(), dto.getAffiliation(), dto.getEmail(), papers);
  }
}
