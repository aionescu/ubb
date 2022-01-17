package ro.arc.packageManager.server.repository;

import ro.arc.packageManager.common.domain.BaseEntity;
import ro.arc.packageManager.common.domain.HelperDB;
import ro.arc.packageManager.common.domain.exceptions.Contract;
import ro.arc.packageManager.common.domain.validators.Validator;
import ro.arc.packageManager.common.domain.validators.ValidatorException;

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class DBRepository<T extends BaseEntity<Long>> implements Repository<Long, T>{
    private HelperDB<T> helperDB;
    private Validator<T> validator;
    private String url;
    private String user ;
    private String password;

    public DBRepository(Validator<T> validator, HelperDB<T> helper, String url, String user, String password) {
        this.validator = validator;
        this.helperDB = helper;
        this.url = url;
        this.password = password;
        this.user = user;
    }

    @Override
    public Optional<T> findOne(Long id) {
        Contract.notNull(id, "id");

        BaseEntity<Long> entity = null;
        String sqlString = "select * from " +this.helperDB.tableName() + " where id = "+id;
        try(var connection = DriverManager.getConnection(this.url, this.user, this.password);
            var ps = connection.prepareStatement(sqlString);
            var rs = ps.executeQuery()) {
            while(rs.next())
            {
                entity = this.helperDB.createFromDB(rs);
            }


        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }

        return Optional.ofNullable((T) entity);
    }

    @Override
    public Iterable<T> findAll() {
        Map<Long, T> entities = new HashMap<>();
        String sqlString = "select * from " +this.helperDB.tableName();
        try(var connection = DriverManager.getConnection(this.url, this.user, this.password);
            var ps = connection.prepareStatement(sqlString);
            var rs = ps.executeQuery()) {
            while(rs.next())
            {
                var entity = this.helperDB.createFromDB(rs);
                entities.put(entity.getID(), (T) entity);
            }


        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
        return entities.entrySet().stream().map(entry -> entry.getValue()).collect(Collectors.toSet());
    }

    @Override
    public Optional<T> save(T entity) throws ValidatorException {
        Contract.notNull(entity, "entity");
        validator.validate(entity);

        String sqlString = "insert into "+this.helperDB.tableName() + " "+this.helperDB.insertQuery(entity);

        try (var connection = DriverManager.getConnection(url, user, password);

             var statement = connection.createStatement()) {

            statement.execute(sqlString);

        } catch (SQLException throwables) {
            throwables.printStackTrace();
            entity = null;
        }
        return Optional.ofNullable(entity);
    }

    @Override
    public Optional<T> delete(Long id) {
        Contract.notNull(id, "id");
        var result = this.findOne(id);

        String sqlString = "delete from "+this.helperDB.tableName()+" where id = "+id;
        try (var connection = DriverManager.getConnection(url, user, password);

             var statement = connection.createStatement()) {

            statement.execute(sqlString);

        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }

        return result;
    }

    @Override
    public Optional<T> update(T entity) throws ValidatorException {
        Contract.notNull(entity, "entity");

        validator.validate(entity);

        String sqlString = "update "+this.helperDB.tableName()+" set ";
        sqlString += this.helperDB.updateQuery(entity);
        try (var connection = DriverManager.getConnection(url, user, password);

             var statement = connection.createStatement()) {

            statement.execute(sqlString);

        } catch (SQLException throwables) {
            throwables.printStackTrace();
            entity = null;
        }

        return Optional.ofNullable(entity);

    }
}

