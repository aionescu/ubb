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
import static utils.uparsec.Parser.*;
import utils.uparsec.Unit;

public final class TLParser {
  private static Parser<Stmt> _mkParser() {
    var multiLineFwdRef = Parser.<Unit>fwdRef();
    var multiLine = multiLineFwdRef.fst;

    var multiLine_ = string("{-")._and(multiLine.or(anyChar.skip()).manyTill(string("-}"))).skip();
    multiLineFwdRef.snd.set(multiLine_);

    var singleLine = string("--")._and(anyChar.manyTill(newline)).skip();

    var comment = singleLine.or(multiLine);
    var ws = spaces._and(comment._and(spaces).many()).skip();

    Parser<Function<Integer, Integer>> sign = ch('-').map_(i -> -i);
    var number = digit.many1().map(List::asString).map(Integer::parseInt);
    Parser<Val> int_ = ap(sign.option(i -> i), number).map(Int::of);

    Parser<Val> bool_ = choice(
      string("True").map_(true),
      string("False").map_(false)
    ).map(Bool::of);

    var val = int_.or(bool_);

    var fstChar = letter.or(ch('_'));
    var sndChar = fstChar.or(digit.or(ch('\'')));

    var ident = liftA2(List::cons, fstChar, sndChar.many()).and_(ws).map(List::asString).map(Ident::of);

    var type = choice(
      string("Int").map_(Type.INT),
      string("Bool").map_(Type.BOOL));

    var opMul = Parser.<BinaryOperator<Expr>>choice(
      ch('*').map_((a, b) -> Arith.of(a, Arith.Op.MUL, b)),
      ch('/').map_((a, b) -> Arith.of(a, Arith.Op.DIV, b)),
      ch('%').map_((a, b) -> Arith.of(a, Arith.Op.REM, b))
    ).and_(ws);

    var opAdd = Parser.<BinaryOperator<Expr>>choice(
      ch('+').map_((a, b) -> Arith.of(a, Arith.Op.ADD, b)),
      ch('-').map_((a, b) -> Arith.of(a, Arith.Op.SUB, b))
    ).and_(ws);

    var opComp = Parser.<BinaryOperator<Expr>>choice(
      string("<=").map_((a, b) -> Comp.of(a, Comp.Op.LTE, b)),
      string(">=").map_((a, b) -> Comp.of(a, Comp.Op.GTE, b)),
      string("<>").map_((a, b) -> Comp.of(a, Comp.Op.NEQ, b)),
      ch('<').map_((a, b) -> Comp.of(a, Comp.Op.LT, b)),
      ch('>').map_((a, b) -> Comp.of(a, Comp.Op.GT, b)),
      ch('=').map_((a, b) -> Comp.of(a, Comp.Op.EQ, b))
    ).and_(ws);

    var opLogic = Parser.<BinaryOperator<Expr>>choice(
      string("and").map_((a, b) -> Logic.of(a, Logic.Op.AND, b)),
      string("or").map_((a, b) -> Logic.of(a, Logic.Op.OR, b))
    ).and_(ws);

    var exprFwdRef = Parser.<Expr>fwdRef();
    var expr = exprFwdRef.fst;

    Parser<Expr> termMul = choice(
      expr.between(ch('(').and_(ws), ch(')').and(ws)),
      val.map(Lit::of),
      ident.map(Var::of)
    ).and_(ws);

    var termAdd = termMul.chainl1(opMul);
    var termComp = termAdd.chainl1(opAdd);
    var termLogic = termComp.chainl1(opComp);

    var termFinal = termLogic.chainl1(opLogic);
    exprFwdRef.snd.set(termFinal);

    Parser<Stmt> print = string("print").and_(ws)._and(expr).map(Print::of);

    var colon = ws._and(ch(':'))._and(ws);
    Parser<Stmt> decl = liftA2(Decl::of, ident.and_(colon), type);

    var arrow = ws._and(string("<-"))._and(ws);
    Parser<Stmt> assign = liftA2(Assign::of, ident.and_(arrow), expr);

    Parser<Stmt> declAssign = liftA3(DeclAssign::of, ident.and_(colon), type.and_(arrow), expr);

    var stmtFwdRef = Parser.<Stmt>fwdRef();
    var stmt = stmtFwdRef.fst;

    var block = ch('{')._and(ws)._and(stmt).and_(ws).and_(ch('}')).and_(ws);

    var ifCond = string("if")._and(ws)._and(expr).and_(ws);
    var elseBlock = string("else")._and(ws)._and(block).option(Nop.nop);

    Parser<Stmt> if_ = liftA3(If::of, ifCond, block, elseBlock);

    var whileCond = string("while")._and(ws)._and(expr).and_(ws);
    Parser<Stmt> while_ = liftA2(While::of, whileCond, block);

    var stmt_ = choice(while_, if_, declAssign, assign, decl, print).and_(ws);
    var compound = stmt_.chainr1(ch(';').and_(ws).map_(Compound::of)).option(Nop.nop);
    stmtFwdRef.snd.set(compound);

    var program = ws._and(stmt).and_(eof);
    return program;
  }

  private static final Parser<Stmt> _parser = _mkParser();

  public static Stmt parse(String code) {
    return _parser.parse(code);
  }
}
