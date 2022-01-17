package ro.arc.packageManager.core.domain.validators;

import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.PackageMaintainer;

@Component
public class PackageMaintainerValidator implements Validator<PackageMaintainer> {
    @Override
    public void validate(PackageMaintainer entity) throws ValidatorException {
        Validate.notNull(entity.getImportance(), "importance");
    }
}