
package ro.arc.packageManager.web.dto;

import lombok.*;

@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
@Builder
public class PackageVersionDto extends BaseDto{
    private String versionNumber;
    private String startingDate;

    private Long packageID;
}
