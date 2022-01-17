package ro.arc.packageManager.core.domain;

import lombok.*;

import javax.persistence.CascadeType;
import javax.persistence.Entity;
import javax.persistence.ManyToOne;

import java.util.Date;

import static java.lang.Long.parseLong;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@EqualsAndHashCode(callSuper = true)
@ToString(callSuper = true)
public class PackageVersion extends BaseEntity<Long> {
    private String versionNumber;
    private String startingDate;

    @ManyToOne(cascade = CascadeType.DETACH)
    private Package aPackage;
}