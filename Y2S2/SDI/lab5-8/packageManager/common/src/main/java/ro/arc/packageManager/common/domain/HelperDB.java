package ro.arc.packageManager.common.domain;

import java.sql.ResultSet;
import java.sql.SQLException;

public abstract class HelperDB<T extends BaseEntity<Long>> {

    public abstract String insertQuery(T entity);

    public abstract BaseEntity<Long> createFromDB(ResultSet rs) throws SQLException;

    public abstract String tableName();

    public abstract String columnNames();

    public abstract String updateQuery(T entity);
}

