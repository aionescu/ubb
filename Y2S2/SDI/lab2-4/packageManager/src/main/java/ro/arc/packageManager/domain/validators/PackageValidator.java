package ro.arc.packageManager.domain.validators;

import ro.arc.packageManager.domain.Package;

public class PackageValidator implements Validator<Package> {
  @Override
  public void validate(Package entity) throws ValidatorException {
    Validate.notNull(entity.getName(), "name");
    Validate.validIdentifier(entity.getName(), "name");

    Validate.notNull(entity.getDescription(), "description");
    Validate.notBlank(entity.getDescription(), "description");

    Validate.notNull(entity.getSourceRepo(), "sourceRepo");
    Validate.validURI(entity.getSourceRepo(), "sourceRepo");

    Validate.notNull(entity.getLicense(), "license");
    Validate.notBlank(entity.getLicense(), "license");
  }
}
