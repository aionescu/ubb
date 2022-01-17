package ro.klan.conferenceManager.service.admin;

import ro.klan.conferenceManager.repository.AdminRepository;
import ro.klan.conferenceManager.service.mapper.admin.AdminMapper;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class AdminServiceImpl implements AdminService {
  private final AdminRepository adminRepository;
  private final AdminMapper adminMapper;
}
