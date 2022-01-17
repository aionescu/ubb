package ro.arc.packageManager.core.domain;

import lombok.*;

import javax.persistence.Entity;

import static java.lang.Long.parseLong;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class PackageVersion extends BaseEntity<Long> {
    private Long packageID;
    private String versionNumber;
}