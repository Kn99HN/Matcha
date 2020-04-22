object VC {
    import md.ast._
    import scala.collection.mutable.ListBuffer

    //@ToDo: Convert to list of appropriate types
    var tests = new ListBuffer[AST]()
    var frehsMap:Map[String,Int] = Map()

    def True: Expr = BConst(true)
    def False: Expr = BConst(false) 
    
    def genCond(prog : AST) : Com = 
        (prog) match {
            case (Seq(c1, c2)) =>
                genCond(c1)
                genCond(c2)
            case If(b, c1, c2) => 
                tests +=
                    (Choice (
                        Seq (Assume (b), genCond(c1)),
                        Seq(Assume(UnOp(Not, b)), genCond(c2))
                    ))
            case (Skip) => tests += (Assume (True))
            case (_) => Assum (True) // change this later
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
}   