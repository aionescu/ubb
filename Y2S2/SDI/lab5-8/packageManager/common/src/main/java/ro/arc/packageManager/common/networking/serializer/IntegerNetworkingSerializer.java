package ro.arc.packageManager.common.networking.serializer;

public class IntegerNetworkingSerializer implements NetworkingSerializer<Integer>{
    @Override
    public String serialize(Integer entity) {
        return String.valueOf(entity);
    }

    @Override
    public Integer deserialize(String string) {
        return Integer.parseInt(string);
    }
}
