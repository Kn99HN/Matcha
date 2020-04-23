object parser{
    import org.scalatest._
    import dzufferey.smtlib._
    import dzufferey.smtlib.{Not => SMTNot}
    import md.ast._
    import md.ast.{Not => MyNot}
    def parse(expr : Expr) : Formula =
        (expr) match {
            case (AConst (x)) => return (IntLit(x))
            case (BConst (x)) => return (Literal(x))
            case (Var (x)) => return Variable(x)
            case (UnOp (op, e)) => 
                (op) match {
                    case (MyNot) => return SMTNot ((parse(e)).setType(Bool))
                    case (UMinus) =>
                        (e) match {
                            case (AConst (x)) => return IntLit(-x)
                        }
                }
            

        }
}