package ro.arc.packageManager.domain;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

import ro.arc.packageManager.domain.exceptions.AppException;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public abstract class BaseEntity<ID> {
  private ID id;

  public BaseEntity(ID id) {
    this.id = id;
  }

  public ID getID() {
    return id;
  }

  public void setID(ID id) {
    this.id = id;
  }

  private static Stream<Method> getGetters(Class<?> cls) {
    return
      Arrays.stream(cls.getDeclaredMethods())
      .filter(m -> m.getName().startsWith("get") && m.getParameterCount() == 0);
  }

  private static String getFieldName(Method getter) {
    var withoutGet = getter.getName().substring(3);
    var uncapitalized = withoutGet.substring(0, 1).toLowerCase() + withoutGet.substring(1);

    return uncapitalized;
  }

  private static Object getFieldValue(Method getter, Object receiver) {
    try {
      return getter.invoke(receiver);
    } catch (Exception e) {
      System.err.println("Reflection error:");
      e.printStackTrace();
      throw new RuntimeException(e);
    }
  }

  private static Object parseField(String text, Class<?> fieldClass) {
    try {
      return
        fieldClass == Long.class
        ? Long.parseLong(text)
        : text;
    } catch (Exception e) {
      throw new AppException("Invalid CSV.");
    }
  }

  public static <T extends BaseEntity<Long>> T ofCSVRow(String text, Class<T> cls) {
    var cols = text.split(",");

    Long id = Long.parseLong(cols[0]);
    var fieldCols = Arrays.asList(cols).subList(1, cols.length);
    var getters = getGetters(cls).collect(Collectors.toList());

    var fieldVals =
      IntStream.range(0, getters.size())
      .mapToObj(i -> parseField(fieldCols.get(i), getters.get(i).getReturnType()))
      .collect(Collectors.toList());

    fieldVals.add(0, id);

    List<Class<?>> ctorArgsList = getters.stream().map(m -> m.getReturnType()).collect(Collectors.toList());
    ctorArgsList.add(0, Long.class);

    var ctorArgs = new Class<?>[ctorArgsList.size()];
    ctorArgsList.toArray(ctorArgs);

    try {
      var ctor = cls.getDeclaredConstructor(ctorArgs);
      return ctor.newInstance(fieldVals.toArray());
    } catch (Exception e) {
      throw new AppException("Invalid CSV.");
    }
  }

  public String toCSVRow() {
    var cls = getClass();

    var fields = getGetters(cls).map(m -> getFieldValue(m, this).toString());
    var idField = getID().toString();

    return fields.reduce(idField, (a, b) -> a + "," + b);
  }

  @Override
  public final String toString() {
    var cls = getClass();
    var idField = "id = " + getID();

    var fields =
      getGetters(cls)
      .map(m -> getFieldName(m) + " = " + getFieldValue(m, this));

    return String.format(
      "%s { %s }",
      cls.getSimpleName(),
      fields.reduce(idField, (a, b) -> a + ", " + b));
  }

  @Override
  public final boolean equals(Object o) {
    if (o == null)
      return false;

    if (this == o)
      return true;

    var cls = getClass();

    if (cls != o.getClass())
      return false;

    if (!getID().equals(((BaseEntity<?>)o).getID()))
      return false;

    return
      getGetters(cls)
      .allMatch(g -> getFieldValue(g, this).equals(getFieldValue(g, o)));
  }

  @Override
  public final int hashCode() {
    var cls = getClass();

    var fields = new ArrayList<Object>();
    fields.add(cls);
    fields.add(getID());

    getGetters(cls).map(m -> getFieldValue(m, this)).collect(Collectors.toCollection(() -> fields));

    return Objects.hash(fields.toArray());
  }

  public abstract Node toXML(Document document);
}
