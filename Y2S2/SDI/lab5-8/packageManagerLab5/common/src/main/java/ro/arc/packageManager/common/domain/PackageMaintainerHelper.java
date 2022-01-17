package ro.arc.packageManager.common.domain;

import java.sql.ResultSet;
import java.sql.SQLException;

public class PackageMaintainerHelper extends HelperDB<PackageMaintainer> {
    @Override
    public String insertQuery(PackageMaintainer entity) {
        return " (packageID, maintainerID) VALUES ("+entity.getPackageID()+", "+entity.getMaintainerID()+" )";
    }

    @Override
    public BaseEntity<Long> createFromDB(ResultSet rs) throws SQLException {
        Long id = rs.getLong("id");
        Long packageID = rs.getLong("packageID");
        Long maintainerID = rs.getLong("maintainerID");

        return new PackageMaintainer(id, maintainerID, packageID);
    }

    @Override
    public String tableName() {
        return "PackageMaintainer";
    }

    @Override
    public String columnNames() {
        return "(id, packageID, maintainerID)";
    }

    @Override
    public String updateQuery(PackageMaintainer entity) {
        return "packageID = "+entity.getPackageID()+", maintainerID = "+entity.getMaintainerID()+" where id = "+entity.getID();
    }
}
