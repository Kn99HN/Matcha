object WP {
    import md.ast._
    import scala.collection.mutable.ListBuffer

    //map for freshvariables
    var frehsMap:Map[String,Int] = Map()
    def True: Expr = BConst(true)
    
    def computeWPs(coms: List[Com]) : List[Expr] = {
        var output = new ListBuffer[Expr]()
        computeWPs(coms, output).toList
    }
    def computeWPs(coms : List[Com], output : ListBuffer[Expr]) : ListBuffer[Expr] = 
        (coms) match {
            case (x :: xs) => 
                output += (computeWP(x, True))
                computeWPs(xs, output)
            case Nil => output
        }

    //computing weakest preconditions
    def computeWP(com : Com, expr : Expr) : Expr =
        (com) match {
            case(Assume (b)) => return BinOp(Impl, b, expr)
            case(Assert (b)) => 
                return BinOp(And, b, expr)
            case (Havoc (b)) => 
                var fresh = getFreshVar(b)
                return replaceExpression(b, fresh, expr)
            case(Seq (c1, c2)) =>
                computeWP(c1, computeWP(c2, expr))
            case(Choice (c1, c2)) =>
                BinOp(And, computeWP(c1, expr), computeWP(c2, expr))

        }

    def computeWLP(com : Com, expr : Expr) : Expr =
        (com) match {
            case(Assume (b)) => return BinOp(Impl, b, expr)
            case(Assert (b)) => return BinOp(Impl, b, expr)
            case (Havoc (b)) => 
                var fresh = getFreshVar(b)
                return replaceExpression(b, fresh, expr)
            case(Seq (c1, c2)) =>
                computeWP(c1, computeWP(c2, expr))
            case(Choice (c1, c2)) =>
                BinOp(And, computeWP(c1, expr), computeWP(c2, expr))

        }

    // @ToDo: Rethink this logic to be more functional
    def getFreshVar(curr : String) : String = {
        if(!frehsMap.contains(curr)) {
            frehsMap += (curr -> 1)
            return curr + "a1"
        } 
        var freshIdx = frehsMap.get(curr).get
        freshIdx += 1
        frehsMap += (curr -> freshIdx)
        return curr + 'a' + freshIdx.toString()
    }

    //replace current variable with a fresh one in the expression in question
    def replaceExpression(curr: String, fresh: String, expr: Expr) : Expr = 
        (expr) match {
            case (Var (name)) => 
                if(name == curr) {
                    return (Var (fresh))
                } 
                return (Var (name))
            case BinOp(op, e1, e2) => BinOp (op, replaceExpression (curr, fresh, e1), replaceExpression(curr, fresh, e2))
            case UnOp(op, e) => UnOp(op, replaceExpression(curr, fresh, e))
            case (_) => expr
        }
        

}