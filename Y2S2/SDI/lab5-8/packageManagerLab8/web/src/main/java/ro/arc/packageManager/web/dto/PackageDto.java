
package ro.arc.packageManager.web.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class PackageDto extends BaseDto{
        private String name;
        private String description;
        private String sourceRepo;
        private String license;
}
