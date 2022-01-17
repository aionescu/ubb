package ro.arc.packageManager.core.domain.exceptions;

public class SerializeException extends AppException{
    public SerializeException(String message) {
        super(message);
    }

    public SerializeException(String message, Throwable cause){
        super(message, cause);
    }

    public SerializeException(Throwable cause)
    {
        super(cause);
    }
}
