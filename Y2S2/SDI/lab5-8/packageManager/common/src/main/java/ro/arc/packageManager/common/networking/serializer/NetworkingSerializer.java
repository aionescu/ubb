package ro.arc.packageManager.common.networking.serializer;

public interface NetworkingSerializer<T> {

    String serialize(T entity);
    T deserialize(String string);
}
