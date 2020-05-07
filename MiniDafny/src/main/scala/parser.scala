object parser{
    import scala.collection.mutable.ListBuffer
    import scala.collection.immutable.Map
    import scala.collection.immutable.HashMap
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
        Or => SMTOr,
        Exists => SMTExists
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
        Or => MyOr,
        Exists => MyExists
        }
    
    val Array = new UnInterpreted("intarray")
    val select = new UnInterpretedFct("select", Some(Array ~> Int ~> Int))
    val update = new UnInterpretedFct("update", Some(Array ~> Int ~> Int ~> Array))
    def True: Expr = BConst(true)
    var GlobalMaps = new HashMap[String, Boolean]()

    

    def combineParse(prog: Program) : Unit = {
        val solver = Z3(UFLIA, "output.txt")

        var methods = prog.methods

        //generating guarded commands
        var gc = GC.genCondProgram(methods)
        var wp = WP.computeWPs(gc)
        
        println("GC are \n")
        for(i <- gc) println(i.pretty + "\n")
        var negWP = List[Expr]()
        for(i <- wp) negWP = UnOp(MyNot, i) :: negWP
        var vc = parseVC(negWP)
        var result = solver.testWithModel(vc)
        println(result)

        // var wp2 = UnOp(MyNot, wp)
        // var parsed2 = parse(wp2)

        // var result = solver.testWithModel(parsed2)
        // mapsConstants(result)
        // findError()
        // println(result)
    }

    def parseVC(expr: List[Expr]) : List[Formula] = {
        var output = new ListBuffer[Formula]()
        parseVC(expr, output).toList
    }

    def parseVC(expr: List[Expr], output: ListBuffer[Formula]) : ListBuffer[Formula] = {
        (expr) match {
            case Nil => output
            case (x :: xs) => 
                (output += parse(x))
                parseVC(xs, output)
        }
    }

    //parse into Z3 Language using SMT-LIB interface
    //figuring out grammar and syntax for method application
    def parse(expr : Expr) : Formula =
        (expr) match {
            case (AConst (x)) => return (IntLit(x))
            case (BConst (x)) => return (Literal(x))
            case (Var (x)) => 
                if(x == "aa1") {
                    return Variable(x).setType(Array)
                }
                return Variable(x).setType(Int)
            case (UnOp (op, e)) => 
                (op) match {
                    case (MyNot) => return SMTNot ((parse(e)).setType(Bool))
                    case (UMinus) =>
                        (e) match {
                            case (AConst (x)) => return IntLit(-x)
                            case (_) => return parse(e)
                        }
                }
            case (BinOp (op, e1, e2)) =>
                (op) match {
                    // binary op with int
                    case MyPlus => return SMTPlus(parse(e1).setType(Int), parse(e2).setType(Int))
                    case MyMinus => return SMTMinus(parse(e1).setType(Int), parse(e2).setType(Int))
                    case MyTimes => return SMTTimes(parse(e1).setType(Int), parse(e2).setType(Int))
                    case Div => 
                        var denom = parse(e2).setType(Int)
                        (denom) match {
                            case (IntLit (x)) => 
                                if(x == 0) throw new Exception("Runtime Error. Cannot divide by 0")
                                else return Divides(parse(e1).setType(Int), parse(e2).setType(Int))
                        }

                    //comparison
                    case MyEq => 
                        return SMTEq(parse(e1).setType(Int), parse(e2).setType(Int))
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
            case (Select (name, e)) =>
                return select(Variable(name).setType(Array), parse(e))
            case(Update (name, i, ei)) =>
                var index = parse(i)
                var value = parse(ei)
                return select(update(Variable(name).setType(Array), index, value), index)
            case (Binder (b, xs, e)) =>
                (b) match {
                    case Forall => 
                        var list = convertList(xs)
                        return ForAll(list, parse(e))
                    case MyExists => return SMTExists(convertList(xs), parse(e))
                }
            case (expr) => parse(expr)
        }

    
    //not handlinng while yet
    def findError() = {
        if(GlobalMaps.contains("Assert")) {
            var bool = GlobalMaps.get("Assert")
            (bool) match {
                case (Some (b)) => 
                    if (!b) println("Post condition might not hold")
                    else Unit
                case (None) => Unit
            }
        }
    }
        

    def mapsConstants(r : Result) = 
        (r) match {
            case (Sat(None)) => Unit
            case (Sat(Some (m))) =>
                println("Satisfiable but not a valid program")
                m.getClass.getDeclaredFields foreach { f =>
                    f.setAccessible(true)
                    if(f.getName == "constants") {
                        var maps = f.get(m).asInstanceOf[Map[Variable, ValDef]]
                        for ((k,v) <- maps) {
                            (k,v) match {
                                case (Variable (name), ValB (b)) =>
                                    GlobalMaps += name -> b
                                case (Variable (name), _) => Unit
                            }
                        }
                    }
                }
            case (UnSat) => println("Valid program\n")
        }
        

    def convertList(list : List[String]) : List[Variable] = {
        var output = new ListBuffer[Variable]()
        for(str <- list) {
            var tmp = Variable(str).setType(Int)
            output += tmp
        }
        return output.toList
    }
}