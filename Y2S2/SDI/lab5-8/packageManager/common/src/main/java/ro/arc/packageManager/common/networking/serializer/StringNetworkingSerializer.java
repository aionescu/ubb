package ro.arc.packageManager.common.networking.serializer;

public class StringNetworkingSerializer implements NetworkingSerializer<String>{
    @Override
    public String serialize(String entity) {
        return entity;
    }

    @Override
    public String deserialize(String string) {
        return string;
    }
}
