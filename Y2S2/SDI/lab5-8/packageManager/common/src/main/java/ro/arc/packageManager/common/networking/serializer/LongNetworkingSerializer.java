package ro.arc.packageManager.common.networking.serializer;

public class LongNetworkingSerializer implements NetworkingSerializer<Long>{
    @Override
    public String serialize(Long entity) {
        return String.valueOf(entity);
    }

    @Override
    public Long deserialize(String string) {
        return Long.parseLong(string);
    }
}
