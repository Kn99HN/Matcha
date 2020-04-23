object parser{
    import org.scalatest._
    import dzufferey.smtlib._
    import dzufferey.smtlib.{
        Not => SMTNot,
        Plus => SMTPlus,
        Minus => SMTMinus,
        Times => SMTTimes,
        Eq => SMTEq,
        Lt => SMTLt,
        Gt => SMTGt,
        And => SMTAnd,
        Or => SMTOr
        }
    import md.ast._
    import md.ast.{
        Not => MyNot,
        Plus => MyPlus,
        Minus => MyMinus,
        Times => MyTimes,
        Div => MyDiv,
        Eq => MyEq,
        Lt => MyLt,
        Gt => MyGt,
        And => MyAnd,
        Or => MyOr
        }
        
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
            case (BinOp (op, e1, e2)) =>
                (op) match {
                    // binary op with int
                    case MyPlus => return SMTPlus(parse(e1).setType(Int), parse(e2).setType(Int))
                    case MyMinus => return SMTMinus(parse(e1).setType(Int), parse(e2).setType(Int))
                    case MyTimes => return SMTTimes(parse(e1).setType(Int), parse(e2).setType(Int))
                    case Div => return Divides(parse(e1).setType(Int), parse(e2).setType(Int))

                    //comparison
                    case MyEq => return SMTEq(parse(e1).setType(Int), parse(e2).setType(Int))
                    case Ne => return SMTNot (SMTEq(parse(e1).setType(Int), parse(e2).setType(Int)))
                    case MyLt => return SMTLt(parse(e1).setType(Int), parse(e2).setType(Int))
                    case Le => return Leq(parse(e1).setType(Int), parse(e2).setType(Int))
                    case MyGt => return SMTGt(parse(e1).setType(Int), parse(e2).setType(Int))
                    case Ge => return Geq(parse(e1).setType(Int), parse(e2).setType(Int))

                    case MyAnd => return SMTAnd(parse(e1).setType(Bool), parse(e2).setType(Bool))
                    case MyOr => return SMTOr(parse(e1).setType(Bool), parse(e2).setType(Bool))
                    case Impl => return Implies(parse(e1), parse(e2))
                    case Iff => 
                        return SMTAnd(Implies(parse(e1), parse(e2)), Implies(parse(e2), parse(e1)))

                }
            

        }
}