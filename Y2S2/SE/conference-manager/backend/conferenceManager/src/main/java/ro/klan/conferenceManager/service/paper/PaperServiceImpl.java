package ro.klan.conferenceManager.service.paper;

import ro.klan.conferenceManager.repository.PaperRepository;
import ro.klan.conferenceManager.service.mapper.paper.PaperMapper;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class PaperServiceImpl implements PaperService {
    private final PaperRepository paperRepository;
    private final PaperMapper paperMapper;
}
