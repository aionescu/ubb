package ro.klan.conferenceManager.service.chair;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import ro.klan.conferenceManager.repository.ChairRepository;
import ro.klan.conferenceManager.service.mapper.chair.ChairMapper;

@Service
@AllArgsConstructor
public class ChairServiceImpl implements ChairService {
  private final ChairRepository chairRepository;
  private final ChairMapper chairMapper;
}
