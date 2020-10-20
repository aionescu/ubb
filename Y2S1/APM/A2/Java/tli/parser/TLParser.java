package tli.parser;

import java.util.function.BinaryOperator;
import java.util.function.Function;

import tli.ast.Ident;
import tli.ast.expr.*;
import tli.ast.stmt.*;
import tli.ast.type.Type;
import tli.ast.val.*;
import utils.collections.list.List;
import utils.uparsec.Parser;
import utils.uparsec.Unit;

public final class TLParser {
  private static Parser<Stmt> _mkParser() {
    var multiLineFwdRef = Parser.<Unit>fwdRef();
    var multiLine = multiLineFwdRef.fst;

    var multiLine_ = Parser.string("{-")._and(multiLine.or(Parser.anyChar().skip()).manyTill(Parser.string("-}"))).skip();
    multiLineFwdRef.snd.set(multiLine_);

    var singleLine = Parser.string("--")._and(Parser.anyChar().manyTill(Parser.newline())).skip();

    var comment = singleLine.or(multiLine);
    var ws = Parser.spaces()._and(comment._and(Parser.spaces()).many()).skip();

    Parser<Function<Integer, Integer>> sign = Parser.ch('-').map_(i -> -i);
    var number = Parser.digit().many1().map(List::asString).map(Integer::parseInt);
    Parser<Val> int_ = Parser.ap(sign.option(i -> i), number).map(Int::of);

    Parser<Val> bool_ = Parser.choice(
      Parser.string("True").map_(true),
      Parser.string("False").map_(false)
    ).map(Bool::of);

    var val = int_.or(bool_);

    var fstChar = Parser.letter().or(Parser.ch('_'));
    var sndChar = fstChar.or(Parser.digit()).or(Parser.ch('\''));

    var ident = Parser.liftA2(List::cons, fstChar, sndChar.many()).and_(ws).map(List::asString).map(Ident::of);

    var type = Parser.choice(
      Parser.string("Int").map_(Type.INT),
      Parser.string("Bool").map_(Type.BOOL));

    var opMul = Parser.<BinaryOperator<Expr>>choice(
      Parser.ch('*').map_((a, b) -> Arith.of(a, Arith.Op.MUL, b)),
      Parser.ch('/').map_((a, b) -> Arith.of(a, Arith.Op.DIV, b)),
      Parser.ch('%').map_((a, b) -> Arith.of(a, Arith.Op.REM, b))
    ).and_(ws);

    var opAdd = Parser.<BinaryOperator<Expr>>choice(
      Parser.ch('+').map_((a, b) -> Arith.of(a, Arith.Op.ADD, b)),
      Parser.ch('-').map_((a, b) -> Arith.of(a, Arith.Op.SUB, b))
    ).and_(ws);

    var opComp = Parser.<BinaryOperator<Expr>>choice(
      Parser.string("<=").map_((a, b) -> Comp.of(a, Comp.Op.LTE, b)),
      Parser.string(">=").map_((a, b) -> Comp.of(a, Comp.Op.GTE, b)),
      Parser.string("<>").map_((a, b) -> Comp.of(a, Comp.Op.NEQ, b)),
      Parser.ch('<').map_((a, b) -> Comp.of(a, Comp.Op.LT, b)),
      Parser.ch('>').map_((a, b) -> Comp.of(a, Comp.Op.GT, b)),
      Parser.ch('=').map_((a, b) -> Comp.of(a, Comp.Op.EQ, b))
    ).and_(ws);

    var opLogic = Parser.<BinaryOperator<Expr>>choice(
      Parser.string("and").map_((a, b) -> Logic.of(a, Logic.Op.AND, b)),
      Parser.string("or").map_((a, b) -> Logic.of(a, Logic.Op.OR, b))
    ).and_(ws);

    var exprFwdRef = Parser.<Expr>fwdRef();
    var expr = exprFwdRef.fst;

    Parser<Expr> termMul = Parser.choice(
      expr.between(Parser.ch('(').and_(ws), Parser.ch(')').and(ws)),
      val.map(Lit::of),
      ident.map(Var::of)
    ).and_(ws);

    var termAdd = termMul.chainl1(opMul);
    var termComp = termAdd.chainl1(opAdd);
    var termLogic = termComp.chainl1(opComp);

    var termFinal = termLogic.chainl1(opLogic);
    exprFwdRef.snd.set(termFinal);

    Parser<Stmt> print = Parser.string("print").and_(ws)._and(expr).map(Print::of);

    var colon = ws._and(Parser.ch(':'))._and(ws);
    Parser<Stmt> decl = Parser.liftA2(Decl::of, ident.and_(colon), type);

    var arrow = ws._and(Parser.string("<-"))._and(ws);
    Parser<Stmt> assign = Parser.liftA2(Assign::of, ident.and_(arrow), expr);

    var stmtFwdRef = Parser.<Stmt>fwdRef();
    var stmt = stmtFwdRef.fst;

    var block = Parser.ch('{')._and(ws)._and(stmt).and_(ws).and_(Parser.ch('}')).and_(ws);

    var ifCond = Parser.string("if")._and(ws)._and(expr).and_(ws);
    var elseBlock = Parser.string("else")._and(ws)._and(block).option(Nop.nop);

    var ifCondF = ifCond.<Function<Stmt, Function<Stmt, Stmt>>>map(c -> b -> e -> If.of(c, b, e));
    Parser<Stmt> if_ = Parser.ap(Parser.ap(ifCondF, block), elseBlock);

    var whileCond = Parser.string("while")._and(ws)._and(expr).and_(ws);
    Parser<Stmt> while_ = Parser.liftA2(While::of, whileCond, block);

    var stmt_ = Parser.choice(while_, if_, assign, decl, print).and_(ws);
    var compound = stmt_.chainr1(Parser.ch(';').and_(ws).map_(Compound::of)).option(Nop.nop);
    stmtFwdRef.snd.set(compound);

    var program = ws._and(stmt).and_(Parser.eof());
    return program;
  }

  private static final Parser<Stmt> _parser = _mkParser();

  public static Stmt parse(String code) {
    return _parser.parse(code);
  }
}
