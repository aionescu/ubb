
package ro.arc.packageManager.web.dto;

import lombok.*;

import java.util.Set;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class PackagesDto {
    private Set<PackageDto> packages;
}

