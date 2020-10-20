package utils.collections.map;

import java.util.HashMap;
import java.util.Optional;

import utils.collections.list.List;

public final class Map<K, V> {
  private final HashMap<K, V> _hm;

  public static <K, V> Map<K, V> empty() {
    return new Map<>(new HashMap<>());
  }

  private Map(HashMap<K, V> hm) {
    _hm = hm;
  }

  public Map<K, V> insert(K k, V v) {
    var hm = new HashMap<>(_hm);
    hm.put(k, v);

    return new Map<>(hm);
  }

  public Optional<V> lookup(K k) {
    return _hm.containsKey(k) ? Optional.of(_hm.get(k)) : Optional.empty();
  }

  @Override
  public String toString() {
    var entries = List.ofStream(_hm.entrySet().stream());

    return
      entries
      .map(e -> e.getKey() + " <- " + e.getValue())
      .toString()
      .replace("[]", "{ }")
      .replace("[", "{ ")
      .replace("]", " }");
  }
}
