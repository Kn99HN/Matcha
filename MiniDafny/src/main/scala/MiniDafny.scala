object MiniDafny extends md.util.MDApp {
  import md.ast.{BConst, Expr, parse}
  import md.util._
  import md.analysis._
  import scala.util.parsing.input.NoPosition
  import dzufferey.smtlib._
  import org.scalatest._

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
      println("Parsed program:\n")
      println(prog.pretty)
    }

    if(verif) {
      println("Verifying the following program:\n")
      println(prog.pretty + "\n")
      print("Number of invalid methods: ")
      parser.combineParse(prog)
    }   
   
    handle(fail()) {
      typeCheck(prog)
    }
   
    
  }
} 
