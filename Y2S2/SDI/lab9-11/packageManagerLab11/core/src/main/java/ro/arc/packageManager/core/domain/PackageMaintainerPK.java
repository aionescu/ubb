package ro.arc.packageManager.core.domain;

import lombok.*;

import java.io.Serializable;

@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
@EqualsAndHashCode
public class PackageMaintainerPK implements Serializable {
    private Maintainer maintainer;
    private Package apackage;
}
