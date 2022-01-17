package ro.arc.packageManager.common.domain.validators;

import ro.arc.packageManager.common.domain.PackageVersion;

public class PackageVersionValidator implements Validator<PackageVersion> {
    @Override
    public void validate(PackageVersion entity) throws ValidatorException {
        Validate.notNull(entity.getPackageID(), "packageID");
        Validate.notNegative(entity.getPackageID(), "packageID");

        Validate.notNull(entity.getVersionNumber(), "versionNumber");
        Validate.validVersionNumber(entity.getVersionNumber(), "versionNumber");
    }
}
