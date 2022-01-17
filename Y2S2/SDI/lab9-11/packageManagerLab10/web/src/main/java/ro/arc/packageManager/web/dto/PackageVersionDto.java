
package ro.arc.packageManager.web.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class PackageVersionDto extends BaseDto{
    private Long packageID;
    private String versionNumber;
}
