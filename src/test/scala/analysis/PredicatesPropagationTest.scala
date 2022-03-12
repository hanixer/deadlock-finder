package deadlockFinder
package analysis

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HilToLil, LilToSsa, SourceToHil, AssertInsertion}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser
import deadlockFinder.analysis.{UsesAndDefs, VarInfo}
import deadlockFinder.hil.IntLiteral

class PredicatesPropagationTest extends AnyFunSuite:

  test("example 2") {
    val source = """
public class Example2 {
    static void func1() {
        int rank = mpi.MPI.COMM_WORLD.Rank();
        int x = 6;
        if (rank == 0) {
            x++;
        } else if (rank == 1) {
            x--;
        }
    }
}
"""
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    println(PrettyPrint(func))
    println("==============")
    val result = PredicatesPropagation.propagate(func)
    for ((k, v) <- result) do
      println(s"$k: $v")
    assert(1 === 1)
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
    println(PrettyPrint(func))
    println("==============")
    val result = PredicatesPropagation.propagate(func)
    for ((k, v) <- result) do
      println(s"$k: $v")
    assert(1 === 1)
  }
