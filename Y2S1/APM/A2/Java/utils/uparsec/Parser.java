package utils.uparsec;

import java.util.Arrays;
import java.util.function.BiFunction;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.function.Predicate;

import utils.Pair;
import utils.collections.list.List;
import utils.uparsec.exn.FwdRefAlreadySetException;
import utils.uparsec.exn.ParserStuckException;
import utils.uparsec.exn.ParsingFailedException;

@FunctionalInterface
public interface Parser<A> {
  public static final class FwdRef<A> {
    private Parser<A> _p;

    public void set(Parser<A> p) {
      if (_p != null)
        throw new FwdRefAlreadySetException();

      _p = p;
    }
  }

  Result<A> run(String input);

  public default A parse(String input) {
    return this.run(input).match(
      () -> { throw new ParsingFailedException(); },
      (a, rest) -> a
    );
  }

  public static <A> Pair<Parser<A>, FwdRef<A>> fwdRef() {
    var fwdRef = new FwdRef<A>();
    Parser<A> p = s -> fwdRef._p.run(s);

    return Pair.of(p, fwdRef);
  }

  public static <A> Parser<A> fail() {
    return s -> Result.fail();
  }

  public static <A> Parser<A> pure(A a) {
    return s -> Result.of(a, s);
  }

  public default <B> Parser<B> map(Function<A, B> f) {
    return s -> this.run(s).match(Result::fail, (a, s2) -> Result.of(f.apply(a), s2));
  }

  public default <B> Parser<B> map_(B b) {
    return map(a -> b);
  }

  public static <A, B, C> Parser<C> liftA2(BiFunction<A, B, C> f, Parser<A> pa, Parser<B> pb) {
    return pa.and(pb).map(Pair.match(f));
  }

  public static <A, B, C, D> Parser<D> liftA3(TriFunction<A, B, C, D> f, Parser<A> pa, Parser<B> pb, Parser<C> pc) {
    return ap(ap(pa.map(f.curried()), pb), pc);
  }

  public static <A, B> Parser<B> ap(Parser<Function<A, B>> pf, Parser<A> pa) {
    return liftA2(Function::apply, pf, pa);
  }

  public default <B> Parser<B> bind(Function<A, Parser<B>> f) {
    return s -> this.run(s).match(Result::fail, (a, s2) -> f.apply(a).run(s2));
  }

  public static final Parser<Character> anyChar =
    s -> s.isEmpty() ? Result.fail() : Result.of(s.charAt(0), s.substring(1));

  public static Parser<Character> satisfy(Predicate<Character> p) {
    return anyChar.bind(c -> p.test(c) ? pure(c) : fail());
  }

  public static final Parser<Character> digit = satisfy(Character::isDigit);
  public static final Parser<Character> letter = satisfy(Character::isLetter);
  public static final Parser<Character> newline = satisfy(c -> c == '\n' || c == '\r');
  public static final Parser<Unit> spaces = s -> Result.of(Unit.UNIT, s.replaceFirst("^\\s+", ""));
  public static final Parser<Unit> eof = s -> s.isEmpty() ? Result.of(Unit.UNIT, s) : Result.fail();

  public static Parser<Character> ch(char c) {
    return satisfy(c2 -> c == c2);
  }

  public static Parser<String> string(String string) {
    return s -> s.startsWith(string) ? Result.of(string, s.substring(string.length())) : Result.fail();
  }

  public default Parser<A> option(A a) {
    return s -> this.run(s).match(() -> Result.of(a, s), Result::of);
  }

  public default Parser<Unit> skip() {
    return this.map_(Unit.UNIT);
  }

  public default <B> Parser<Pair<A, B>> and(Parser<B> pb) {
    return s -> this.run(s).match(
      Result::fail,
      (a, s2) -> pb.run(s2).match(
        Result::fail,
        (b, s3) -> Result.of(Pair.of(a, b), s3)));
  }

  public default <B> Parser<A> and_(Parser<B> pb) {
    return this.and(pb).map(Pair::fst_);
  }

  public default <B> Parser<B> _and(Parser<B> pb) {
    return this.and(pb).map(Pair::snd_);
  }

  public default Parser<A> or(Parser<A> p) {
    return s -> this.run(s).match(() -> p.run(s), Result::of);
  }

  public static <A> Parser<A> choice(List<Parser<A>> ps) {
    return ps.match(Parser::fail, (a, as) -> a.or(choice(as)));
  }

  @SafeVarargs
  public static <A> Parser<A> choice(Parser<A>... ps) {
    return choice(List.ofStream(Arrays.stream(ps)));
  }

  public default <Pre, Post> Parser<A> between(Parser<Pre> pre, Parser<Post> post) {
    return pre._and(this).and_(post);
  }

  private static <A> Parser<A> _ensureConsumes(Parser<A> p) {
    return s -> p.run(s).match(
      Result::fail,
      (a, rest) -> {
        if (rest.length() == s.length())
          throw new ParserStuckException();

        return Result.of(a, rest);
      }
    );
  }

  private static <A> Parser<List<A>> _many(Parser<A> p, List<A> acc) {
    return s -> p.run(s).match(() -> Result.of(acc, s), (a, s2) -> _many(p, List.cons(a, acc)).run(s2));
  }

  public default Parser<List<A>> many() {
    return _many(_ensureConsumes(this), List.nil()).map(List::reverse);
  }

  private static <A> Parser<List<A>> _nonEmpty(Parser<List<A>> p) {
    return p.bind(l -> l.match(Parser::fail, (h, t) -> Parser.pure(l)));
  }

  public default Parser<List<A>> many1() {
    return _nonEmpty(many());
  }

  public default <Sep> Parser<List<A>> sepBy(Parser<Sep> sep) {
    return this.and(sep._and(this).many()).map(Pair.match(List::cons));
  }

  public default <Sep> Parser<List<A>> sepBy1(Parser<Sep> sep) {
    return _nonEmpty(sepBy(sep));
  }

  private static <A, End> Parser<List<A>> _manyTill(Parser<A> p, Parser<End> end, List<A> acc) {
    return s -> end.run(s).match(
      () -> p.run(s).match(() -> Result.of(acc, s), (a, s2) -> _manyTill(p, end, List.cons(a, acc)).run(s2)),
      (a, s2) -> Result.of(acc, s2));
  }

  public default <End> Parser<List<A>> manyTill(Parser<End> p) {
    return _manyTill(_ensureConsumes(this), p, List.nil()).map(List::reverse);
  }

  public default <End> Parser<List<A>> many1Till(Parser<End> end) {
    return _nonEmpty(manyTill(end));
  }

  public default Parser<A> chainl1(Parser<BinaryOperator<A>> op) {
    return liftA2((a, l) -> l.foldl((s, p) -> p.fst.apply(s, p.snd), a), this, op.and(this).many());
  }

  public default Parser<A> chainr1(Parser<BinaryOperator<A>> op) {
    return liftA2((l, a) -> l.foldr((p, s) -> p.snd.apply(p.fst, s), a), this.and(op).many(), this);
  }
}
