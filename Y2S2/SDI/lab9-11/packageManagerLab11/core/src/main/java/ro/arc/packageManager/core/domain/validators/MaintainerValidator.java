package ro.arc.packageManager.core.domain.validators;


import org.springframework.stereotype.Component;
import ro.arc.packageManager.core.domain.Maintainer;

@Component
public class MaintainerValidator implements Validator<Maintainer> {

    @Override
    public void validate(Maintainer entity) throws ValidatorException {
        Validate.notNull(entity.getUserName(), "userName");
        Validate.validIdentifier(entity.getUserName(), "userName");

        Validate.notNull(entity.getFullName(), "fullName");
        Validate.notBlank(entity.getFullName(), "fullName");

        Validate.notNull(entity.getEmail(), "email");
        Validate.validEmail(entity.getEmail(), "email");
    }
}

