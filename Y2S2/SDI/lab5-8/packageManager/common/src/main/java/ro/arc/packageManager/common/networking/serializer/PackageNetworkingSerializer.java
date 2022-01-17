package ro.arc.packageManager.common.networking.serializer;

import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.Package;

public class PackageNetworkingSerializer implements NetworkingSerializer<Package> {
  @Override
  public String serialize(Package entity) {
    return entity.toCSVRow();
  }

  @Override
  public Package deserialize(String string) {
    return BaseEntity.ofCSVRow(string, Package.class);
  }
}
