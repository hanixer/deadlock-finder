package deadlockFinder
package analysis

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{AssertInsertion, HilToLil, LilToSsa, SourceToHil}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser
import deadlockFinder.analysis.{UsesAndDefs, VarInfo}
import deadlockFinder.expr.*
import deadlockFinder.lil.FuncDecl

class PredicatesPropagationTest extends AnyFunSuite:
  private def printCodeAndResult(func: FuncDecl, result: Map[String, List[Int]]): Unit =
    println(PrettyPrint(func))
    println("==============")
    for ((label, ranks) <- result) do println(s"$label: ${ranks.mkString(", ")}")

  private def translateAndPropagate(source: String) =
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val result = PredicatesPropagation.propagate(func)
    (func, result)
    // TODO: refresh tests
/*
  test("simple") {
    val source = """
public class Simple {
    static void func1() {
        int rank = mpi.MPI.COMM_WORLD.Rank();
        int x = 6;
        if (rank == 0) {
            x = 100;
        } else if (rank == 1) {
            x = 200;
        }
    }
}
"""
    val (func, result) = translateAndPropagate(source)
    printCodeAndResult(func, result)
    assert(result("bb5") === List(1))
    assert(result("bb2") === List(0))
    assert(result("bb6") === List(0, 1, 2, 3, 4))
    assert(result("bb7") === List(0, 1, 2, 3, 4))
    assert(result("bb4") === List(0, 1, 2, 3, 4))
  }

  test("more conditions") {
    val source = """
public class More {
    static void func1() {
        int rank = mpi.MPI.COMM_WORLD.Rank();
        int x = 6;
        int y = 3;
        if (rank == 0) {
          x = x + y;
        }
        else if (rank == 1) {
          x = y - x;
        }
        else {
          x = x + x;
        }
    }
}
"""
    val (func, result) = translateAndPropagate(source)
    printCodeAndResult(func, result)
    assert(result("bb5") === List(1))
    assert(result("bb2") === List(0))
    assert(result("bb6") === List(0, 1, 2, 3, 4))
    assert(result("bb7") === List(0, 1, 2, 3, 4))
    assert(result("bb4") === List(0, 1, 2, 3, 4))
  }

  test("ranks from predicates") {
    import PredicatesPropagation.Predicate
    assert(
      PredicatesPropagation.ranksFromPredicates(
        List(Predicate(0, false), Predicate(1, true), Predicate(2, false)),
        5
      ) === List(1)
    )
    assert(
      PredicatesPropagation.ranksFromPredicates(
        List(Predicate(0, false), Predicate(2, false)),
        5
      ) === List(1, 3, 4)
    )
  }
*/