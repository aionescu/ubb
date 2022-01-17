package ro.arc.packageManager.common.domain.validators;

import ro.arc.packageManager.common.domain.exceptions.AppException;

public class ValidatorException extends AppException {
    private static final long serialVersionUID = 1;

    public ValidatorException(String message) {
        super(message);
    }

    public ValidatorException(String message, Throwable cause) {
        super(message, cause);
    }

    public ValidatorException(Throwable cause) {
        super(cause);
    }
}
