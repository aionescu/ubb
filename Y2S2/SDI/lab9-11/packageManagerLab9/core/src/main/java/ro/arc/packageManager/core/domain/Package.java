package ro.arc.packageManager.core.domain;

import lombok.*;

import javax.persistence.Entity;

@Entity
@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class Package extends BaseEntity<Long>  {
    private String name;
    private String description;
    private String sourceRepo;
    private String license;
}
