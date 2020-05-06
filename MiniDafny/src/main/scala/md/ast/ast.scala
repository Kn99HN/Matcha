package md.ast

import scala.util.parsing.input.Positional

sealed abstract class AST extends Positional {
  // pretty print as AST
  override def toString(): String = print.prettyAST(this)
  // pretty print as MiniDafny expression
  def pretty(): String = print.pretty(this)
}
  
sealed abstract class Expr extends AST
  
/* Literals and Variables */
  case class AConst(n: Int) extends Expr
  case class BConst(b: Boolean) extends Expr
  case class Var(x: String) extends Expr

  /* Unary and Binary Operations */
  case class BinOp(op: Bop, e1: Expr, e2: Expr) extends Expr
  case class UnOp(op: Uop, e: Expr) extends Expr

  /* Array expressions */
  case class Select(x: String, e: Expr) extends Expr /* x[e] */
  case class Update(x: String, i: Expr, ei: Expr) extends Expr /* x[i := ei] */
  
  
  /* Quantifiers */
  case class Binder(b: BinderKind, xs: List[String], e: Expr) extends Expr
  
  /* Quantifier kinds */
  sealed abstract class BinderKind extends Positional
  case object Exists extends BinderKind
  case object Forall extends BinderKind

  /* Functions */
  case class Method(name: String, arguments: List[Expr], returnVal: Expr, pre: Expr, c: Com, post: Expr) extends Expr
  case class MethodApplication(name: String, arguments: List[Expr]) extends Expr
  
  /* Binary Operators */
  sealed abstract class Bop
  case object Plus extends Bop /* + */
  case object Minus extends Bop /* - */
  case object Times extends Bop /* * */
  case object Div extends Bop /* / */

  case object Eq extends Bop /* == */
  case object Ne extends Bop /* != */
  case object Lt extends Bop /* < */
  case object Le extends Bop /* <= */
  case object Gt extends Bop /* > */
  case object Ge extends Bop /* >= */
  
  case object And extends Bop /* && */
  case object Or extends Bop /* || */
  case object Impl extends Bop /* ==> */
  case object Iff extends Bop /* <=> */
  
  /* Unary operators */
  sealed abstract class Uop
  case object UMinus extends Uop /* - */
  case object Not extends Uop /* ! */

  /* Commands */
  sealed abstract class Com extends AST
  case object Skip extends Com /* skip */
  case class Assign(x: String, e: Expr) extends Com /* x := e */
  case class Havoc(x: String) extends Com /* havoc x */
  case class Assume(e: Expr) extends Com /* assume e */
  case class Assert(e: Expr) extends Com /* assert e */
  case class Choice(c1: Com, c2: Com) extends Com /* c1 [] c2 */
  case class Seq(c1: Com, c2: Com) extends Com /* c1; c2 */
  case class If(b: Expr, c1: Com, c2: Com) extends Com /* if(b) c1 else c2 */
  case class While(b: Expr, inv: Expr, c: Com) extends Com /* while (b) invariant inv c */
  case object Argument extends Com

  /* Programs */
  case class Program(p: String, methods: List[Method]) extends AST
  
  /* Method -> will only handle one argument for now. Will change later */
  // case class Method(n: Var, p : Program) extends AST /* method m(x : int) return (x : int) */
  
  /* Types */
  sealed abstract class Typ {
    // pretty print as AST
    override def toString(): String = print.prettyAST(this)
    // pretty print as JS expression
    def pretty(): String = print.prettyTyp(this)
  }
  case object TInt extends Typ
  case object TBool extends Typ
  case object TArray extends Typ
  case object TUnit extends Typ

object Make {
  def True: Expr = BConst(true)
  
  def False: Expr = BConst(false)
    
  def and(e1: Expr, e2: Expr): Expr =
    (e1, e2) match {
      case (BConst(b), e2) => 
        if (b) e2 else e1
      case (e1, BConst(b)) =>
        if (b) e1 else e2
      case _ => BinOp(And, e1, e2)
    }
  
  def or(e1: Expr, e2: Expr): Expr =
    (e1, e2) match {
      case (BConst(b), e2) => 
        if (b) e1 else e2
      case (e1, BConst(b)) =>
        if (b) e2 else e1
      case _ => BinOp(Or, e1, e2)
    }
}