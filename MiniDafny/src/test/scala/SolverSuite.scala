import org.scalatest._
import dzufferey.smtlib._

class SolverSuite extends FunSuite {

  val p = Variable("p").setType(Bool)
  val q = Variable("q").setType(Bool)

  val x = Variable("x").setType(Int)
  val y = Variable("y").setType(Int)
  val z = Variable("z").setType(Int)
  val t = IntLit(2).setType(Int)

  val Array = new UnInterpreted("intarray")
  
  val a = Variable("a").setType(Array)
  
  val select = new UnInterpretedFct("select", Some(Array ~> Int ~> Int))
  val update = new UnInterpretedFct("update", Some(Array ~> Int ~> Int ~> Array))
  
  test("checking Z3") {
    // create solver instance
    val solver = Z3(UFLIA)

    // a simple test on a propositional formula
    val form1 = And(p, Or(Not(p), q))
    
    assert( solver.testB(form1), "sat formula")
    
    // check validity of 0 < x + y && x == 0 ==> 0 <= y
    val form2 = 
      And(Lt(IntLit(0), Plus(x, y)), Eq(IntLit(0), x), Leq(y, IntLit(0)))
    assert(!solver.testB(form2), "unsat formula")

    // Array select/update axioms
    val arrayAx1 =
      ForAll(List(a, x, y, z), Implies(Eq(x, z), Eq(select(update(a, x, y), z), y)))
    val arrayAx2 =
      ForAll(List(a, x, y, z), Or(Eq(x, z), Eq(select(update(a, x, y), z), select(a, z))))
    
    // prove some simple property on arrays
    val form3 =
      And(Eq(select(a, x), IntLit(1)), Eq(x, IntLit(3)), Eq(select(update(a, IntLit(0), IntLit(2)), x), IntLit(2)))
      
    assert(solver.test(List(arrayAx1, arrayAx2, form3)) === UnSat)
  }
}