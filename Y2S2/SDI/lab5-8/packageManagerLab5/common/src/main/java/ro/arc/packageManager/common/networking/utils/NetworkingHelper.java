package ro.arc.packageManager.common.networking.utils;

import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.Package;
import ro.arc.packageManager.common.domain.PackageMaintainer;
import ro.arc.packageManager.common.domain.PackageVersion;
import ro.arc.packageManager.common.domain.exceptions.AppException;
import ro.arc.packageManager.common.networking.Message;
import ro.arc.packageManager.common.networking.serializer.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NetworkingHelper {

    private static Map<Class<?>, NetworkingSerializer<?>> serializers = new HashMap<>();

    static
    {
        serializers.put(Maintainer.class, new MaintainerNetworkingSerializer());
        serializers.put(Package.class, new PackageNetworkingSerializer());
        serializers.put(PackageMaintainer.class, new PackageMaintainerNetworkingSerializer());
        serializers.put(PackageVersion.class, new PackageVersionNetworkingSerializer());
        serializers.put(Long.class, new LongNetworkingSerializer());
        serializers.put(Integer.class, new IntegerNetworkingSerializer());
        serializers.put(String.class, new StringNetworkingSerializer());
    }

    private NetworkingHelper()
    {
    }

    public static Message exception(String exceptionMessage)
    {
        return new Message(Message.MessageError, exceptionMessage);
    }

    public static void checkException(Message message) throws AppException
    {
        if (message.getHeader().equals(Message.MessageError))
        {
            List<String> messageBody = message.getBody();
            if (messageBody.size() != 1)
            {
                throw new RuntimeException("Received response can't be understood");
            }
            throw new AppException(messageBody.get(0));
        }
    }

    public static boolean isSuccess(Message message)
    {
        return message.getHeader().equals(Message.MessageSuccess);
    }

    public static Message success(List<String> value)
    {
        Message message = new Message(Message.MessageSuccess);
        if (value != null)
            value.forEach(message::addRow);
        return message;
    }

    @SuppressWarnings("unchecked")
    public static <T> T deserialize(String string, Class<T> aClass)
    {
        NetworkingSerializer<?> serializer = serializers.get(aClass);
        if (serializer == null) throw new AppException("Class impossible to deserialize");
        return (T) serializer.deserialize(string);
    }

    @SuppressWarnings({"unchecked"})
    public static <T> String serialize(T entity)
    {
        var aClass = entity.getClass();
        if(! serializers.containsKey(aClass))
            throw new AppException("Class impossible to deserialize");
        NetworkingSerializer<T> serializer = (NetworkingSerializer<T>) serializers.get(aClass);
        return serializer.serialize(entity);
    }

}

