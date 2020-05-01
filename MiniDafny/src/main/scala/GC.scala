object GC {
    import md.ast._
    import scala.collection.mutable.ListBuffer

    var list = new ListBuffer[Com]()
    var frehsMap:Map[String,Int] = Map()
    var modVars = new ListBuffer[String]()

    def True: Expr = BConst(true)
    def False: Expr = BConst(false) 

    //Computing the verification condition for a program
    def genCondProg(prog : Program) : Com = 
        (prog) match {
            case (_) =>
                Seq (
                    genCondPre(prog.pre),
                    Seq (
                        genCondBody(prog.c),
                        genCondPost(prog.post)
                    ))
        }

    def genCondBody(comm : Com) : Com = 
        (comm) match {
            case (Skip) => Assume (True)
            case (Assign (name, expr)) =>
                var freshName = getFreshVar(name)
                Seq(
                    Assume(BinOp(Eq, Var (freshName), Var (name))),
                    Seq(
                        Havoc(name),
                        Assume(BinOp(Eq, Var (name), 
                        replaceExpression(name, freshName, expr)))
                    )
                )
            case (Seq(c1, c2)) =>
                Seq(
                    genCondBody(c1),
                    genCondBody(c2)
                )
            case If(b, c1, c2) => 
                    (Choice (
                        Seq (Assume (BinOp(And, b, Var ("Then"))), genCondBody(c1)),
                        Seq(Assume(BinOp(And, UnOp(Not, b), Var ("Else"))), genCondBody(c2))
                    ))
            case While(b, inv, c) =>
                findModifiedVariables(c) //get array of modified variables
                var havocs = convertModVars(modVars.toList) //convert using array of modified variables
                Seq(
                    Assert (inv),
                    Seq(havocs, 
                    Seq(Assume (inv), Choice(
                        Seq(Assume (b),
                        Seq (genCondBody(c),
                        Seq(Assert (inv),Assume (False)))),
                        Assume (UnOp(Not, b)))))
                )
            case (_) => comm
        }
    
    def genCondPre(pre : Expr) : Com = 
        (pre) match {
            case (_) => Assume (pre)
        }
    
    def genCondPost(post: Expr) : Com =
        (post) match {
            case (_) => Assert (BinOp(Or, (post), Var ("Assert")))
        }
    
    //assume there is always body to the while loop
    def convertModVars(modifiedVars : List[String]) : Com =
        (modifiedVars) match {
            case head :: tail => 
                (tail) match {
                    case Nil => Havoc (head)
                    case (_) => 
                        Seq (Havoc (head), convertModVars(tail))
                }
                
        }
    
    def findModifiedVariables(com : Com) : ListBuffer[String] =
        (com) match {
            case(Skip) => return modVars
            case(Seq (c1, c2)) =>
                findModifiedVariables(c1)
                findModifiedVariables(c2)
                return modVars
            case If(b, c1, c2) => 
                findModifiedVariables(c1)
                findModifiedVariables(c2)
                return modVars
            case Assign(x, expr) =>
                modVars += x
                return modVars
            case (_) => return modVars

        } 

    // @ToDo: Rethink this logic to be more functional
    def getFreshVar(curr : String) : String = {
        if(!frehsMap.contains(curr)) {
            frehsMap += (curr -> 0)
            return curr + '0'
        } 
        var freshIdx = frehsMap.get(curr).get
        freshIdx += 1
        frehsMap += (curr -> freshIdx)
        return curr + freshIdx.toString()
    }

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