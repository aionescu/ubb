package ro.arc.packageManager.common.domain;

import java.sql.ResultSet;
import java.sql.SQLException;

public class PackageHelper extends HelperDB<Package> {
    @Override
    public String insertQuery(Package entity) {
        return " (name, description, sourceRepo, license) VALUES "+"( '"+entity.getName()+"', '"+entity.getDescription()+"', '"+entity.getSourceRepo()+"', '"+entity.getLicense()+"' )";
    }

    @Override
    public String tableName() {
        return "Package";
    }

    @Override
    public String columnNames() {
        return "( id, name, description, sourceRepo, license )";
    }

    @Override
    public String updateQuery(Package entity) {
        return "name = '"+entity.getName()+"', description = '"+entity.getDescription()+"', sourceRepo = '"+entity.getSourceRepo()+"', license = '"+entity.getLicense()+"' where id = "+entity.getID();
    }

    @Override
    public Package mapRow(ResultSet rs, int rowNum) throws SQLException {
        Long id = rs.getLong("id");
        String name = rs.getString("name");
        String description = rs.getString("description");
        String sourceRepo = rs.getString("sourceRepo");
        String license = rs.getString("license");

        return new Package(id, name, description, sourceRepo, license);
    }
}


