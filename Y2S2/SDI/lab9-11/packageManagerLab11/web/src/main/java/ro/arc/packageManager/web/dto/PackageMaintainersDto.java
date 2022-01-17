
package ro.arc.packageManager.web.dto;

import lombok.*;

import java.util.Set;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class PackageMaintainersDto {
  private Set<PackageMaintainerDto> packageMaintainers;
}
