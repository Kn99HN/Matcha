package md.ast

import org.kiama.output.PrettyPrinter

object print extends PrettyPrinter {
  override val defaultIndent = 2
  
  /*
   * Determine precedence level of top-level constructor in an expression
   */  
  def prec(e: AST): Int =
    e match {
      case AConst(_) | BConst(_) => 0
      case Var(_) => 0
      case UnOp(_, _) | Select(_, _) => 1
      case Update(_, _, _) => 2
      case BinOp(bop, _, _) =>
        bop match {
          case Times | Div => 3
          case Plus | Minus => 4
          case Gt | Ge | Lt | Le => 6
          case Eq | Ne => 7
          case And => 11
          case Or => 12
          case Impl => 13
          case Iff => 14
        }
      case Binder(_, _, _) => 15
      case Skip | Assign(_, _) | Assert(_) | Assume(_) | Havoc(_) => 16
      case Seq(_, _) | Choice(_, _) => 17
      case If(_, _, _) | While(_, _, _) => 18
      case Method(_,_,_,_,_,_) => 19
      case Program(_, _) => 20
    }
  
  /* Associativity of binary operators */
  sealed abstract class Assoc
  case object Left extends Assoc
  case object Right extends Assoc
  
  /* 
   * Get associativity of a binary operator 
   * (all current operators are left-associative) 
   */
  def assoc(bop: Bop): Assoc = bop match {
    case Impl => Right
    case _ => Left
  }
  
  def showTyp(typ: Typ): Doc = 
    typ match {
      case TBool => "bool"
      case TInt => "int"
      case TArray => "array"
      case TUnit => "unit"
    }
      
  /*
   * Pretty-print expressions in concrete syntax.
   */
  def show(e: AST): Doc = {
    e match {
      case AConst(n) => n.toString()
      case BConst(b) => b.toString()
      case Var(x) => x
      case eu @ UnOp(uop, e) =>
        val op: Doc => Doc = uop match {
          case UMinus => "-" <+> _
          case Not => "!" <+> _
        }
        op(if (prec(e) < prec(eu)) show(e) else parens(show(e)))
      case BinOp(bop, e1, e2) => {
        val op: Doc = bop match {
          case Plus => " + "
          case Minus => " - "
          case Times => " * "
          case Div => " / "
          case And => " && "
          case Or => " || "
          case Impl => " ==> "
          case Iff => " <=> "
          case Eq => " == "
          case Ne => " != "
          case Lt => " < "
          case Le => " <= "
          case Gt => " > "
          case Ge => " >= "
        }
        def eToDoc(e1: Expr, as: Assoc): Doc =
          if (prec(e1) < prec(e) || prec(e1) == prec(e) && as == assoc(bop)) 
            show(e1) 
          else parens(show(e1))
        eToDoc(e1, Left) <> op <> eToDoc(e2, Right)
      }
      case Select(x, e) => x <> brackets(show(e))
      case Update(x, i, ei) => x <> brackets(show(i) <+> ":=" <+> show(ei))
      case Binder(b, xs, e) =>
        val bd = b match {
          case Forall => "forall"
          case Exists => "exists"
        }
        bd <+> ssep(xs map (x => x: Doc), ",") <+> "::" <+> nest(show(e))
      case Skip => "skip" <> semi
      case Assign(x, e) =>
        e match {
          case Update(x, i, ei) => 
            x <> brackets(show(i)) <+> ":=" <+> nest(show(ei)) <> semi
          case _ =>
            x <+> ":=" <+> nest(show(e)) <> semi
        }
      case Havoc (x) =>
        "havoc" <+> x <> semi
      case Assert(e) =>
        "assert" <+> nest(show(e)) <> semi
      case Assume(e) =>
        "assume" <+> nest(show(e)) <> semi
      case Seq(c1, c2) =>
        show(c1) <> line <> show(c2)
      case Choice(c1, c2) =>
        braces(nest(line <> show(c1)) <> line) <+> "[]" <+>
          braces(nest(line <> show(c2)) <> line)
      case If(b, c1, c2) =>
        "if" <+> parens(show(b)) <+> 
          braces(nest(line <> show(c1)) <> line) <+> 
          (c2 match {
            case Skip => empty
            case _ => "else" <+> braces(nest(line <> show(c2)) <> line)
          })
      case While(b, inv, c) =>
        "while" <+> parens(show(b)) <> 
           nest(line <> "invariant" <+> nest(show(inv))) <> line <>
           braces(nest(line <> show(c)) <> line)
      case Return(name) =>
        "return" <+> name
      case MethodApplication(p, args) =>
        p <+> parens(ssep(args map (x => show(x)), ","))
      case Method(p, args, ret, req, c, ens) => 
        "method" <+> p <+> parens(ssep(args map (x => show(x)), ",")) <+>
          "returns" <+> parens(show(ret)) <>
          nest(line <> "requires" <+> show(req) <> line <> "ensures" <+> show(ens)) <> 
            line <> braces(nest(line <> show(c)) <> line)
      case Program(p, methods) =>
        "program" <+> p <>
          nest(line <> ssep(methods map (x => show(x)), ","))
    }
  }
  
  def prettyAST(x: Any): String = pretty(any(x))
  def pretty(e: AST): String = pretty(show(e))
  def prettyTyp(typ: Typ): String = pretty(showTyp(typ))
}
  
  