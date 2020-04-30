package md.analysis

import md.util._
import md.ast._

object typeCheck {
  /* Type Checker */
  
  case class TypeError(texp: Typ, tbad: Typ, e: AST) extends 
    MDException("Type Error: expected an expression of type " + texp.pretty + " but got " + tbad.pretty(), e.pos)
  
  type TypeEnv = Map[String, Typ]
  
  def typeCheck(e: AST): State[TypeEnv, Typ] = {
    def err[T](texp: Typ, tgot: Typ, e1: AST): T = throw TypeError(texp, tgot, e1)
    def check(tgot: Option[Typ], texp: Typ, e1: AST): Typ = {
      tgot match {
        case None => texp
        case Some(tgot) => 
          if (texp == tgot) texp else err(texp, tgot, e1)
      }
    }
    def typ(texp: Typ, e1: AST): State[TypeEnv, Typ] = {
      for { 
        tgot <- typeCheck(e1)
      } yield check(Some(tgot), texp, e1)
    }
    def getAndCheck(texp: Typ, x: String): State[TypeEnv, Typ] =
      for {
          typx <- State {env: TypeEnv => 
            val typx = check(env get x, texp, e)
            (env + (x -> typx), typx)
          }
      } yield typx
      
    e match {
      case AConst(_) => State.insert(TInt)
      case BConst(_) => State.insert(TBool)
      case Var(x) => getAndCheck(TInt, x)        
      case BinOp(bop, e1, e2) =>
        bop match {
          case Plus | Minus | Times | Div => 
            for {
              _ <- typ(TInt, e1)
              _ <- typ(TInt, e2)
            } yield TInt           
          case And | Or | Iff | Impl =>
            typ(TBool, e1)
            typ(TBool, e2)
          case Lt | Le | Gt | Ge =>
            for {
              _ <- typ(TInt, e1)
              _ <- typ(TInt, e2)
            } yield TBool
          case Eq | Ne =>
            for {
              t1 <- typeCheck(e1)
              _ <- typ(t1, e2)
            } yield TBool           
        }
      case UnOp(uop, e) =>
        uop match {
          case Not => typ(TBool, e)
          case UMinus => typ(TInt, e)
        }
      case Select(x, e) =>
        for {
          _ <- getAndCheck(TArray, x)
          _ <- typ(TInt, e)
        } yield TInt
      case Update(x, i, ei) =>
        for {
          _ <- getAndCheck(TArray, x)
          _ <- typ(TInt, i)
          _ <- typ(TInt, ei)
        } yield TArray
      case Binder(_, xs, e) =>
        for {
          oldEnv <- State.init[TypeEnv]
          _ <- State.write { env: TypeEnv =>
            (xs foldLeft env) { (env, x) => env + (x -> TInt) }
          }
          _ <- typ(TBool, e)
          _ <- State.write { env: TypeEnv =>
              oldEnv ++ (env -- xs)
            }
        } yield TBool
      case Skip => State.insert(TUnit)
      case Assume(e) => 
        for {
          _ <- typ(TBool, e)
        } yield TUnit
      case Assert(e) => 
        for {
          _ <- typ(TBool, e)
        } yield TUnit
      case Havoc(_) => State.insert(TUnit)
      case Assign(x, e@Update(_, _, _)) =>
        for {
          _ <- getAndCheck(TArray, x)
          _ <- typ(TArray, e)
        } yield TUnit
      case Assign(x, e) => 
        for {
          _ <- getAndCheck(TInt, x)
          _ <- typ(TInt, e)
        } yield TUnit
      case Seq(c1, c2) =>
        for {
          _ <- typ(TUnit, c1)
          _ <- typ(TUnit, c2)
        } yield TUnit
      case Choice(c1, c2) =>
        for {
          _ <- typ(TUnit, c1)
          _ <- typ(TUnit, c2)
        } yield TUnit
      case If(b, c1, c2) =>
        for {
          _ <- typ(TBool, b)
          _ <- typ(TUnit, c1)
          _ <- typ(TUnit, c2)
        } yield TUnit
      case While(b, inv, c) =>
        for {
          _ <- typ(TBool, b)
          _ <- typ(TBool, inv)
          _ <- typ(TUnit, c)
        } yield TUnit
      case Argument =>
        State.insert(TUnit)
      case Program(_, _, pre, c, post) =>
        for {
          _ <- typ(TBool, pre)
          _ <- typ(TBool, post)
          _ <- typ(TUnit, c)
        } yield TUnit
    }
  }
  
  def apply(e: AST): Unit = typeCheck(e)(Map.empty)
  
}