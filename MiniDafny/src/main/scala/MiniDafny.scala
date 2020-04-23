object MiniDafny extends md.util.MDApp {
  import md.ast._
  import md.util._
  import md.analysis._
  import scala.util.parsing.input.NoPosition

  def True: Expr = BConst(true)
  
  /* Interface to run your verifier from the command line. */ 
  
  def processFile(file: java.io.File) {
    if (debug) {
      println("Parsing " + file.getName + "...")
    }
    
    val prog = handle(fail()) {
      parse.fromFile(file)
    }
    if (debug) {
      // var list = GC.genCondProg(prog)
      // for(comm <- list) println(comm.pretty)
      var coms = GC.genCondProg(prog)
      println(WP.computeWP(coms, True))
      println("Parsed program:\n")
      println(prog.pretty)
    }  
   
    handle(fail()) {
      typeCheck(prog)
    }
   
    
  }

  // def test(prog : Com) : Unit = 
  //   (prog) match {
  //     case While (b, inv, c) =>
  //         var tmp = VC.findModifiedVariables(c)
  //         println(VC.convertModVars(tmp.toList))
  //   }
} 
