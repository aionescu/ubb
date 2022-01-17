
package ro.arc.packageManager.web.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Builder
public class PackageMaintainerDto extends BaseDto {
  private Long maintainerID;
  private Long packageID;
  private String packageName;
  private Integer importance;
}
