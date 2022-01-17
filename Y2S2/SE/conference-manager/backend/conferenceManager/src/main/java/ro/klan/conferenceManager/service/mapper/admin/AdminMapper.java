package ro.klan.conferenceManager.service.mapper.admin;

import ro.klan.conferenceManager.entity.Admin;
import ro.klan.conferenceManager.service.admin.AdminDto;

public interface AdminMapper {
  AdminDto toService(Admin entity);
  Admin toEntity(AdminDto dto);
}
