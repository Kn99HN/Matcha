object MiniDafny extends md.util.MDApp {
  import md.ast._
  import md.util._
  import md.analysis._
  import scala.util.parsing.input.NoPosition
  import dzufferey.smtlib._
  import org.scalatest._

  def True: Expr = BConst(true)
  
  /* Interface to run your verifier from the command line. */ 
  
  def processFile(file: java.io.File) {
    val solver = Z3(UFLIA)

    if (debug) {
      println("Parsing " + file.getName + "...")
    }
    
    val prog = handle(fail()) {
      parse.fromFile(file)
    }

    if (debug) {
      var gc = GC.genCondProg(prog)
      println(gc.pretty + "\n")
      var wps = WP.computeWP(gc, True)
      println(wps.pretty + "\n")
      var formula = parser.parse(wps)
      println(solver.testB(formula))
      println("Parsed program:\n")
      println(prog.pretty)
    }  
   
    handle(fail()) {
      typeCheck(prog)
    }
   
    
  }
} 

// how to get message using solver
// suppose P ^ !P, send to solver
// Introduce a new boolean and conjunct with
// the negated version
// make it a procedural language
