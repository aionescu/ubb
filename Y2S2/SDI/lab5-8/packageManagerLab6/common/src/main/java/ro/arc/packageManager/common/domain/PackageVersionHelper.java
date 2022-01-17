package ro.arc.packageManager.common.domain;


import java.sql.ResultSet;
import java.sql.SQLException;

import static java.lang.Long.parseLong;

public class PackageVersionHelper extends HelperDB<PackageVersion>{


    @Override
    public String insertQuery(PackageVersion entity) {
        return "(packageID, versionNumber) VALUES "+"( '"+entity.getPackageID()+"', '"+entity.getVersionNumber()+"' )";
    }

    @Override
    public String tableName() {
        return "PackageVersion";
    }

    @Override
    public String columnNames() {
        return "( id, packageID, versionNumber)";
    }

    @Override
    public String updateQuery(PackageVersion entity) {
        return "packageID = '"+entity.getPackageID()+"', versionNumber = '"+entity.getVersionNumber()+"' where id = "+entity.getID();
    }

    @Override
    public PackageVersion mapRow(ResultSet rs, int rowNum) throws SQLException {
        Long id = rs.getLong("id");
        String packageID = rs.getString("packageID");
        String versionNumber = rs.getString("versionNumber");

        return new PackageVersion(id, parseLong(packageID), versionNumber);
    }

}
