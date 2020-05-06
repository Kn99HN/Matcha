package md.ast

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.token._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.input._

object parse extends JavaTokenParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  val reserved: Set[String] =
    Set("true", "false", "skip",
        "assume", "assert", "havoc", 
        "if", "else", "while", 
        "program", "requires", "ensures", "method", "return")
  
  def prog: Parser[Program] =
    positioned("program" ~> ident ~ methods ^^ {
      case name ~ methods => Program(name, methods)
    }) 

  def methods : Parser[List[Method]] = rep(method)

  def method: Parser[Method] =
    positioned("method" ~> ident ~ args ~ ret ~ specs ~ com ^^ {
      case name ~ args ~ ret ~ specs ~ c =>
        val (req, ens) =
          (specs foldLeft (Make.True, Make.True)) {
            case ((req, ens), Left(e)) => (Make.and(req, e), ens)
            case ((req, ens), Right(e)) => (req, Make.and(ens, e))
          }
        Method(name, args, ret, req, c, ens)
    })
        
  def specs: Parser[List[Either[Expr, Expr]]] = rep(spec) 

  def args : Parser[List[Expr]] = rep(arg)

  def arg : Parser[Expr] = 
    positioned("," ~> ident <~ ")" ^^ {
      case last =>
        Var(last)
    }) |
    positioned("," ~> ident ^^ {
      case next => 
        Var(next)
    }) | 
    positioned("(" ~> ident <~ ")" ^^ {
      case va => Var(va)
    }) | 
    positioned("(" ~> ident ^^ {
      case first =>
        Var(first)
    })

  def ret : Parser[Expr] =
    positioned ("return" ~> primaryExpr) ^^ {
      case e => e
    }
    
  def spec: Parser[Either[Expr, Expr]] =
    "requires" ~> expr ^^ { e => Left(e) } |
    "ensures" ~> expr ^^ { e => Right(e) }
  
  def com: Parser[Com] =
    rep(basicCom) ^^
    { case (cmds: List[Com]) => 
        if (cmds == Nil) Skip
        else cmds reduceRight[Com] {
          case (c1, c2) => Seq(c1, c2)
        }
    }
   
  def stmtSep: Parser[String] = ";"
 
  def basicCom: Parser[Com] =
    skipCom |
    assignCom |
    havocCom |
    assertCom |
    assumeCom |
    ifCom |
    whileCom | 
    "{" ~> com <~ "}"
    
  def skipCom: Parser[Com] =
    "skip" ^^^ Skip
    
  def assignCom: Parser[Assign] =
    positioned((ident <~ ":=") ~ expr <~ ";" ^^ { 
      case x~e => 
      Assign(x, e) 
    } ) |
    positioned((ident ~ ("[" ~> expr <~ "]") <~ ":=") ~ expr <~ ";" ^^ { 
      case x~i~ei => Assign(x, Update(x, i, ei)) 
    } ) | 
    positioned(((ident <~ ":=")) ~ methodApp <~ ";" ^^ {
      case x ~ e =>
      Assign(x, e)
    })

  def havocCom: Parser[Havoc] =
    positioned("havoc" ~> ident <~ ";" ^^ { x => Havoc(x) }) 

  def assertCom: Parser[Assert] =
    positioned("assert" ~> expr <~ ";" ^^ { e => Assert(e) }) 
  
  def assumeCom: Parser[Assume] =
    positioned("assume" ~> expr <~ ";" ^^ { e => Assume(e) }) 

  def ifCom: Parser[If] =
    positioned(("if" ~> "(" ~> expr <~ ")") ~ basicCom ~ opt("else" ~> basicCom) ^^ {
      case e~c1~oc2 => If(e, c1, oc2 getOrElse Skip)
    })
    
  def whileCom: Parser[While] =
    positioned(("while" ~> "(" ~> expr <~ ")") ~ rep(invariant) ~ basicCom ^^ {
      case b~invs~c => 
        val inv = (invs foldLeft Make.True) { (e1, e2) => Make.and(e1, e2) }
        While(b, inv, c)
    })
    
  def invariant: Parser[Expr] =
    "invariant" ~> expr 
    
  def expr: Parser[Expr] = 
  binderExpr

  def methodApp : Parser[Expr] =
    positioned(ident ~ args ^^ {
      case name ~ args =>  
      MethodApplication(name, args)
    })
 
  def binderExpr: Parser[Expr] =
    rep(binderKind ~ ident ~ rep("," ~> ident) <~ "::") ~ iffExpr ^^ {
      case qs~e =>
        (qs foldRight e) { case (b~x~xs, e) => Binder(b, x :: xs, e).setPos(b.pos) }
    }
  
  def binderKind: Parser[BinderKind] =
    positioned("forall" ^^^ Forall) |
    positioned("exists" ^^^ Exists)
    
  def iffExpr: Parser[Expr] =
    implExpr ~ rep("<=>" ~> implExpr) ^^
    { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(Iff, e1, e2).setPos(e1.pos) } 
    }
    
  def implExpr: Parser[Expr] =
    rep(orExpr <~ "==>") ~ orExpr ^^
    { case es~e1 => 
        (es foldRight e1) { case (e1, e2) => BinOp(Impl, e1, e2).setPos(e1.pos) } 
    }
   
  def orExpr: Parser[Expr] =
    andExpr ~ rep("||" ~> andExpr) ^^ 
      { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(Or, e1, e2).setPos(e1.pos) } 
      }

  def andExpr: Parser[Expr] =
    eqExpr ~ rep("&&" ~> eqExpr) ^^ 
      { case e1~es => 
        (e1 /: es) { case (e1, e2) => BinOp(And, e1, e2).setPos(e1.pos) } 
      }
  
  def eqOp: Parser[Bop] =
    "==" ^^^ Eq |
    "!=" ^^^ Ne
    
  def eqExpr: Parser[Expr] =
    relExpr ~ rep(eqOp ~ relExpr) ^^ 
      { case e1~opes => 
          (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
  
  def relOp: Parser[Bop] =
    "<="  ^^^ Le |
    "<"   ^^^ Lt |
    ">="  ^^^ Ge |
    ">"   ^^^ Gt
    
  def relExpr: Parser[Expr] =
    additiveExpr ~ rep(relOp ~ additiveExpr) ^^ 
      { case e1~opes => 
          (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      } 
  
  def additiveOp: Parser[Bop] = 
    "+" ^^^ Plus | 
    "-" ^^^ Minus
      
  def additiveExpr: Parser[Expr] =
    multitiveExpr ~ rep(additiveOp ~ multitiveExpr) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
       
  def multitiveOp: Parser[Bop] = 
    "*" ^^^ Times |
    "/" ^^^ Div
  
  def multitiveExpr: Parser[Expr] = 
    selectExpr ~ rep(multitiveOp ~ unaryExpr) ^^ 
      { case e1~opes => 
        (e1 /: opes) { case (e1, op~e2) => BinOp(op, e1, e2).setPos(e1.pos) } 
      }
  
  def selectExpr: Parser[Expr] =
    positioned(ident ~ ("[" ~> expr <~ "]") ^^ { case x~e => Select(x, e) }) |
    unaryExpr
    
  def unaryOp: Parser[Uop] =
    "-" ^^^ UMinus |
    "!" ^^^ Not
     
  def unaryExpr: Parser[Expr] =
    positioned(unaryOp ~ primaryExpr ^^ { case uop~e => UnOp(uop, e) }) |
    primaryExpr      
    
  def primaryExpr: Parser[Expr] = 
    literalExpr |
    positioned(ident ^^ { Var(_) }) |
    "(" ~> expr <~ ")"
    
  override def ident: Parser[String] =
    super.ident ^? ({
      case id if !reserved(id) => id
    }, { id => s"$id is reserved." }) | 
    "$" ~> super.ident ^^ (s => "$" + s)
    
  def literalExpr: Parser[Expr] =
    positioned("true" ^^^ BConst(true)) |
    positioned("false" ^^^ BConst(false)) |
    positioned(wholeNumber ^^ { n => AConst(n.toInt) })
    
  override def stringLiteral: Parser[String] =
    ("\""+"""([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\"").r |
    ("\'"+"""([^'\p{Cntrl}\\]|\\[\\'"bfnrt]|\\[0-7]{3}|\\u[a-fA-F0-9]{2}|\\u[a-fA-F0-9]{4})*"""+"\'").r
  

    
  /** utility functions */
  private def getProg(p: ParseResult[Program]): Program = 
    p match {
      case Success(p, _) => p
        
      case NoSuccess(msg, next) => 
        throw new md.util.MDException(msg, next.pos)
    }
  
  def fromString(s: String) = getProg(parseAll(prog, s))
  
  def fromFile(file: java.io.File) = {
    val reader = new java.io.FileReader(file)
    val result = parseAll(prog, StreamReader(reader))
    getProg(result)
  }
}