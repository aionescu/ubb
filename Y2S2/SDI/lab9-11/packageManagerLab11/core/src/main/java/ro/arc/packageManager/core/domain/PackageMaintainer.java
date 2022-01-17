package ro.arc.packageManager.core.domain;

import lombok.*;

import javax.persistence.*;

import static java.lang.Long.parseLong;

@Entity
@Table(name = "package_maintainer")
@IdClass(PackageMaintainerPK.class)
@NoArgsConstructor
@AllArgsConstructor
@Data
@Getter
@Setter
public class PackageMaintainer  {

    @Id
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "maintainer_id")
    private Maintainer maintainer;

    @Id
    @ManyToOne(optional = false, fetch = FetchType.EAGER)
    @JoinColumn(name = "package_id")
    private Package apackage;

    private int importance;

    @Override
    public String toString() {
        return "PackageMaintainer{" +
                "maintainer=" + maintainer.getId() +
                ", apackage=" + apackage.getId() +
                ", importance=" + importance +
                '}';
    }
}
