package ro.arc.packageManager.core.domain;

import lombok.*;

import javax.persistence.Entity;

import static java.lang.Long.parseLong;

@Entity
@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class PackageMaintainer extends BaseEntity<Long>  {
    private Long maintainerID;
    private Long packageID;
}
