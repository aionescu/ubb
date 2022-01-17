package ro.arc.packageManager.common.domain.validators;

import ro.arc.packageManager.common.domain.PackageMaintainer;

public class PackageMaintainerValidator implements Validator<PackageMaintainer> {
    @Override
    public void validate(PackageMaintainer entity) throws ValidatorException {
        Validate.notNull(entity.getMaintainerID(), "maintainerID");
        Validate.notNegative(entity.getMaintainerID(), "maintainerID");

        Validate.notNull(entity.getPackageID(), "packageID");
        Validate.notNegative(entity.getPackageID(), "packageID");
    }
}