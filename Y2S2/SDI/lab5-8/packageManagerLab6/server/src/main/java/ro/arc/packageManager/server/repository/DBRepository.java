package ro.arc.packageManager.server.repository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcOperations;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.core.RowCallbackHandler;
import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.HelperDB;
import ro.arc.packageManager.common.domain.Maintainer;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.Validator;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.stream.Collectors;

public class DBRepository<T extends BaseEntity<Long>> implements Repository<Long, T>{
    private HelperDB<T> helperDB;
    private Validator<T> validator;

    @Autowired
    private JdbcOperations jdbcOperations;

    public DBRepository(Validator<T> validator, HelperDB<T> helper) {
        this.validator = validator;
        this.helperDB = helper;
    }

    @Override
    public Optional<T> findOne(Long id) {
        Contract.notNull(id, "id");

        String sqlString = "select * from " +this.helperDB.tableName() + " where id = "+id;
        List<T> entities = this.jdbcOperations.query(sqlString, this.helperDB);
        if (entities.size() == 0)
            return Optional.empty();

        return Optional.of(entities.get(0));
    }

    @Override
    public Iterable<T> findAll() {
        String sqlString = "select * from " +this.helperDB.tableName();
        List<T> entities = this.jdbcOperations.query(sqlString, this.helperDB);
        return entities;
    }

    @Override
    public Optional<T> save(T entity) throws ValidatorException {
        Contract.notNull(entity, "entity");
        validator.validate(entity);

        String sqlString = "insert into "+this.helperDB.tableName() + " "+this.helperDB.insertQuery(entity);

        int result = jdbcOperations.update(sqlString);
        return result == 1 ? Optional.empty() : Optional.of(entity);
    }

    @Override
    public Optional<T> delete(Long id) {
        Contract.notNull(id, "id");
        var toDelete = this.findOne(id);

        String sqlString = "delete from "+this.helperDB.tableName()+" where id = "+id;

        int result = jdbcOperations.update(sqlString);
        return result == 1 ? toDelete : Optional.empty();
    }

    @Override
    public Optional<T> update(T entity) throws ValidatorException {
        Contract.notNull(entity, "entity");

        validator.validate(entity);

        Optional<T> toUpdate = this.findOne(entity.getID());

        String sqlString = "update "+this.helperDB.tableName()+" set ";
        sqlString += this.helperDB.updateQuery(entity);
        int result = jdbcOperations.update(sqlString);

        return result == 1 ? Optional.of(toUpdate.get()) : Optional.empty();

    }


}

