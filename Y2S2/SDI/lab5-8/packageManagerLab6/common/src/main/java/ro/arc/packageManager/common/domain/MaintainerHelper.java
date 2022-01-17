package ro.arc.packageManager.common.domain;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

public class MaintainerHelper extends HelperDB<Maintainer> {
    @Override
    public String insertQuery(Maintainer entity) {
        return "(userName, fullName, email) VALUES "+"( '"+entity.getUserName()+"', '"+entity.getFullName()+"', '"+entity.getEmail()+"' )";
    }

    @Override
    public String tableName() {
        return "Maintainer";
    }

    @Override
    public String columnNames() {
        return "( id, userName, fullName, email )";
    }

    @Override
    public String updateQuery(Maintainer entity) {
        return "userName = '"+entity.getUserName()+"', fullName = '"+entity.getFullName()+"', email = '"+entity.getEmail()+"' where id = "+entity.getID();
    }


    @Override
    public Maintainer mapRow(ResultSet rs, int rowNum) throws SQLException {
        Long id = rs.getLong("id");
        String userName = rs.getString("userName");
        String fullName = rs.getString("fullName");
        String email = rs.getString("email");

        return new Maintainer(id, userName, fullName, email);
    }
}


