package ro.arc.packageManager.core.domain.validators;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.PackageVersion;

@Component
public class PackageVersionValidator implements Validator<PackageVersion> {
    @Override
    public void validate(PackageVersion entity) throws ValidatorException {
        Validate.notNull(entity.getVersionNumber(), "versionNumber");
        Validate.validVersionNumber(entity.getVersionNumber(), "versionNumber");

        Validate.notNull(entity.getStartingDate(), "startingDate");
    }
}
