package ro.arc.packageManager.core.domain.validators;

public interface Validator<T> {
    void validate(T entity) throws ValidatorException;
}

