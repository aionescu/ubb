package ro.arc.packageManager.core.domain;


import lombok.*;

import javax.persistence.Entity;

@Entity
@NoArgsConstructor
@AllArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class Maintainer extends BaseEntity<Long> {
    private String userName;
    private String fullName;
    private String email;
}