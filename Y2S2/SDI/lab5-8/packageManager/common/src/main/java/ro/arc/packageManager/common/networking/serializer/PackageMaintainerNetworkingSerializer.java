package ro.arc.packageManager.common.networking.serializer;

import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.PackageMaintainer;

public class PackageMaintainerNetworkingSerializer implements NetworkingSerializer<PackageMaintainer> {
  @Override
  public String serialize(PackageMaintainer entity) {
    return entity.toCSVRow();
  }

  @Override
  public PackageMaintainer deserialize(String string) {
    return BaseEntity.ofCSVRow(string, PackageMaintainer.class);
  }
}
