package deadlockFinder
package analysis

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{AssertInsertion, HilToLil, LilToSsa, SourceToHil}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser
import deadlockFinder.analysis.{UsesAndDefs, VarInfo}
import deadlockFinder.hil.IntLiteral
import deadlockFinder.lil.FuncDecl

class PredicatesPropagationTest extends AnyFunSuite:
  private def printCodeAndResult(func: FuncDecl, result: Map[String, ProcessRank]): Unit = {
    println(PrettyPrint(func))
    println("==============")
    for ((k, v) <- result) do
      println(s"$k: $v")
  }

  test("example 2") {
    val source = """
public class Example2 {
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
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val result = PredicatesPropagation.propagate(func)
    printCodeAndResult(func, result)
    assert(result("bb2") === ProcessRank.Concrete(0))
    assert(result("bb4") === ProcessRank.Concrete(1))
    assert(result("bb3") === ProcessRank.AnyRank)
    assert(result("bb5") === ProcessRank.AnyRank)
  }

  test("example 1") {
    val source = """
public class Example1 {
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
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val result = PredicatesPropagation.propagate(func)
    printCodeAndResult(func, result)
    assert(result("bb2") === ProcessRank.Concrete(0))
    assert(result("bb4") === ProcessRank.Concrete(1))
    assert(result("bb3") === ProcessRank.AnyRank)
    assert(result("bb5") === ProcessRank.AnyRank)
  }
