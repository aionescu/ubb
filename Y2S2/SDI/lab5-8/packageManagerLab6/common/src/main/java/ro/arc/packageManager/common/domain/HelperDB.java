package ro.arc.packageManager.common.domain;

import org.springframework.jdbc.core.RowMapper;

import java.io.Serializable;
import java.sql.ResultSet;
import java.sql.SQLException;

public abstract class HelperDB<T extends BaseEntity<Long>> implements RowMapper<T> {

    public abstract String insertQuery(T entity);

    public abstract String tableName();

    public abstract String columnNames();

    public abstract String updateQuery(T entity);
}

