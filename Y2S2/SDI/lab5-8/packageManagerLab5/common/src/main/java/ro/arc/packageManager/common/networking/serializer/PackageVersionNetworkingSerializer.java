package ro.arc.packageManager.common.networking.serializer;

import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.PackageVersion;

public class PackageVersionNetworkingSerializer implements NetworkingSerializer<PackageVersion> {
    @Override
    public String serialize(PackageVersion entity) {
        return entity.toCSVRow();
    }

    @Override
    public PackageVersion deserialize(String string) {
        return BaseEntity.ofCSVRow(string, PackageVersion.class);
    }
}