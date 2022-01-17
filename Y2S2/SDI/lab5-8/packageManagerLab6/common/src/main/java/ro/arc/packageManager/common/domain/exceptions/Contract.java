package ro.arc.packageManager.common.domain.exceptions;

public class Contract {
    public static void notNull(Object o, String name) throws AppException {
        if (o == null)
            throw new AppException(name + " cannot be null.");
    }

    public static void ensure(boolean condition, String message) throws AppException {
        if (!condition)
            throw new AppException(message);
    }
}
