package ro.arc.packageManager.core.domain.validators;

import java.net.MalformedURLException;
import java.net.URL;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.regex.Pattern;

public class Validate {
    private static final String emailRegex = "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$";
    private static final Pattern emailPattern = Pattern.compile(emailRegex);

    private static final String identifierRegex = "^[a-zA-Z0-9\\-_]+$";
    private static final Pattern identifierPattern = Pattern.compile(identifierRegex);

    private static final String versionNumberRegex = "^(0|[1-9]\\d*)(\\.(0|[1-9]\\d*)){0,3}$";
    private static final Pattern versionNumberPattern = Pattern.compile(versionNumberRegex);
    private static final String dateFormatString  = "dd-mm-yyyy";
    private static final DateFormat dateFormat = new SimpleDateFormat(dateFormatString);

    public static void notNull(Object o, String fieldName) {
        if (o == null)
            throw new ValidatorException("The field " + fieldName + " cannot be null.");
    }

    public static void notBlank(String s, String fieldName) {
        if (s.isBlank())
            throw new ValidatorException("The text field " + fieldName + " cannot be null or blank.");
    }

    public static void notNegative(Long l, String fieldName) {
        if (l < 0)
            throw new ValidatorException("The field " + fieldName + " cannot be negative.");
    }

    public static void validEmail(String text, String fieldName) {
        if (!emailPattern.matcher(text).matches())
            throw new ValidatorException("The field " + fieldName +  " must be a valid email.");
    }

    public static void validIdentifier(String text, String fieldName) {
        if (!identifierPattern.matcher(text).matches())
            throw new ValidatorException("The field " + fieldName + " can only contain letters, digits, _ and -.");
    }

    public static void validURI(String text, String fieldName) {
        try {
            new URL(text);
        } catch (MalformedURLException e) {
            throw new ValidatorException("The field " + fieldName + " must be a valid URI.");
        }
    }

    public static void validVersionNumber(String text, String fieldName) {
        if(!versionNumberPattern.matcher(text).matches())
            throw new ValidatorException("The field " + fieldName +  " must be a valid version number.");
    }

    public static void validDate(String text, String fieldName) {
        dateFormat.setLenient(false);
        try{
            dateFormat.parse(text);
        }
        catch (ParseException e) {
            throw new ValidatorException("The field "+fieldName + " is not a valid date with format dd-mm-yyyy.");
        }
    }
}
