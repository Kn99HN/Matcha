object GC {
    import md.ast._
    import scala.collection.mutable.ListBuffer

    var list = new ListBuffer[Com]()
    var frehsMap:Map[String,Int] = Map()
    var modVars = new ListBuffer[String]()
    var funcVarMaps: Map[String, Int] = Map()
    var functionMap: Map[String, Method] = Map() 

    def True: Expr = BConst(true)
    def False: Expr = BConst(false) 


    /*
        - @TODO: create mapping for function names
    */
    def genFunctionMap(prog: List[Method]) : Unit = 
        (prog) match {
            case (x :: xs) => 
                (x) match {
                    case (Method (name, _, _, _, _, _ )) => functionMap += (name -> x)
                }
                genFunctionMap(xs)
            case Nil => Unit
        }

    def genCondProgram(prog: List[Method]) : List[Com] = {
        var output = new ListBuffer[Com]()
        genFunctionMap(prog)
        genCondProgram(prog, output).toList
    }

    //changing ordering of appending later
    def genCondProgram(prog: List[Method], output: ListBuffer[Com]) : ListBuffer[Com] =
        (prog) match {
            case (x :: xs) => 
                (output += genCondProg(x))
                genCondProgram(xs, output) 
            case Nil => output
        }

    //Computing the verification condition for a method
    def genCondProg(prog : Method) : Com = 
        (prog) match {
            case (_) =>
                Seq (
                    genCondPre(prog.pre),
                    Seq (
                        genCondBody(prog.name, prog.c),
                        genCondPost(prog.post)
                    ))
        }

    //handling null cases
    def genCondBody(callerName: String, comm : Com) : Com = 
        (comm) match {
            case (Skip) => Assume (True)
            case (Assign (name, expr)) =>
                (expr) match {
                    case (MethodApplication (methodName, args)) => {
                        var callee = 
                            (functionMap.get(methodName)) match {
                                case (Some (method)) => method
                            }
                        var caller = 
                            (functionMap.get(callerName)) match {
                                case (Some (caller)) => caller
                            }
                        replaceExpression(args, callee)
                        var newPre = replaceExpression(callee.pre)
                        var fresh = getFreshVar(name)
                        var newPost = replaceExpression(callee.post)
                        var ret = (callee.returnVal) match {
                            case (Var (retName)) => retName
                            case (_) => ""
                        }
                        var newPostRet = replaceExpression(ret, fresh, newPost)
                        Seq(
                            Assert(newPre),
                            Seq(
                                Assume(newPostRet),
                                genCondBody(callerName, Assign(name, Var (fresh)))
                            )
                        )
                    }
                    case (_) => {
                        var freshName = getFreshVar(name)
                        Seq(
                            Assume(BinOp(Eq, Var (freshName), Var (name))),
                            Seq(
                                Havoc(name),
                                Assume(BinOp(Eq, Var (name), 
                                replaceExpression(name, freshName, expr)))
                            )
                        )
                    }
                }
            case (Seq(c1, c2)) =>
                Seq(
                    genCondBody(callerName, c1),
                    genCondBody(callerName , c2)
                )
            case If(b, c1, c2) => 
                    (Choice (
                        Seq (Assume (BinOp(And, b, Var ("Then"))), genCondBody(callerName, c1)),
                        Seq(Assume(BinOp(And, UnOp(Not, b), Var ("Else"))), genCondBody(callerName, c2))
                    ))
            case While(b, inv, c) =>
                findModifiedVariables(c) //get array of modified variables
                var havocs = convertModVars(modVars.toList) //convert using array of modified variables
                Seq(
                    Assert (inv),
                    Seq(havocs, 
                    Seq(Assume (inv), Choice(
                        Seq(Assume (b),
                        Seq (genCondBody(callerName, c),
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
    
    //handling null cases
    def findMethod(progs: List[Method], name: String) : Method =
        (progs) match {
            case (x :: xs) => {
                if(x.name == name) return x
                else findMethod(xs, name)
            }
        }

    //mapping all variables with its values
    def replaceExpression(args: List[Expr], callee: Method) : Unit = {
        var listCallee = callee.arguments
        (listCallee zip args).map{
            case(Var (name), AConst (value)) => 
                funcVarMaps += (name -> value)
            case(_) => Unit
        }
    }

    //replace using the mapping
    def replaceExpression(expr: Expr) : Expr = {
        (expr) match {
            case (Var (name)) => {
                if(funcVarMaps.contains(name)) {
                    var value = funcVarMaps.get(name)
                    (value) match {
                        case (Some (i)) => AConst(i)
                        case (_) => expr
                    }
                }
                else expr
            }
            case(BinOp (op, e1, e2)) => BinOp(op, replaceExpression(e1), replaceExpression(e2))
            case(UnOp (op, e)) => UnOp(op, replaceExpression(e))
            case(_) => expr
        }
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