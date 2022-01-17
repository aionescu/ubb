
package ro.arc.packageManager.web.dto;

import lombok.*;
import ro.arc.packageManager.core.domain.PackageVersion;

import java.util.Set;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class PackageVersionsDto {
    private Set<PackageVersionDto> packageVersions;
}

