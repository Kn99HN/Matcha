object MiniDafny extends md.util.MDApp {
  import md.ast._
  import md.util._
  import md.analysis._
  import scala.util.parsing.input.NoPosition
  
  /* Interface to run your verifier from the command line. */ 
  
  def processFile(file: java.io.File) {
    if (debug) {
      println("Parsing " + file.getName + "...")
    }
    
    val prog = handle(fail()) {
      parse.fromFile(file)
    }
    if (debug) {
      // println(VC.genCond(prog))
      println(prog)
      println("Parsed program:\n")
      println(prog.pretty)
    }  
   
    handle(fail()) {
      typeCheck(prog)
    }
   
    
  }
}
