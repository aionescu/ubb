package ro.arc.packageManager.core.domain;


import lombok.*;

import javax.persistence.*;
import java.util.*;
import java.util.stream.Collectors;

@Entity
@Table(name = "maintainer")
@NoArgsConstructor
@AllArgsConstructor
@Data
public class Maintainer extends BaseEntity<Long> {
    private String userName;
    private String fullName;
    private String email;

    @Embedded
    private Address address;

    @EqualsAndHashCode.Exclude
    @OneToMany(mappedBy = "maintainer", cascade = CascadeType.ALL, fetch =
            FetchType.EAGER)
    private Set<PackageMaintainer> packageMaintainers = new HashSet<>();

    public void addPackage(Package aPackage){
        PackageMaintainer packageMaintainer = new PackageMaintainer();
        packageMaintainer.setApackage(aPackage);
        packageMaintainer.setMaintainer(this);
        this.packageMaintainers.add(packageMaintainer);
    }

    public void addImportance(Package aPackage, Integer importance) {
        PackageMaintainer packageMaintainer = new PackageMaintainer();
        packageMaintainer.setApackage(aPackage);
        packageMaintainer.setImportance(importance);
        packageMaintainer.setMaintainer(this);
        packageMaintainers.add(packageMaintainer);
    }

    public void deletePackage(Long ID){
        this.packageMaintainers.removeIf(packageMaintainer -> packageMaintainer.getApackage().getId().equals(ID));
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Maintainer that = (Maintainer) o;

        return userName.equals(that.userName);
    }

    @Override
    public int hashCode() {
        return userName.hashCode();
    }

    @Override
    public String toString() {
        return "Maintainer{" +
                "userName='" + userName + '\'' +
                ", fullName='" + fullName + '\'' +
                ", email='" + email + '\'' +
                ", address=" + address +
                '}' + super.toString();
    }
}