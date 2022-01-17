package ro.arc.packageManager.common.networking.serializer;

import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.Maintainer;

public class MaintainerNetworkingSerializer implements NetworkingSerializer<Maintainer> {
  @Override
  public String serialize(Maintainer entity) {
    return entity.toCSVRow();
  }

  @Override
  public Maintainer deserialize(String string) {
    return BaseEntity.ofCSVRow(string, Maintainer.class);
  }
}
