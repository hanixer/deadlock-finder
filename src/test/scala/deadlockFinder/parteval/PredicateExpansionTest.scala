package deadlockFinder
package parteval

import analysis.{UsesAndDefs, VarInfo}
import cfg.CfgGraph
import common.PrettyPrint
import expr.*
import parteval.Division
import translation.{HilToLil, LilToSsa, SourceToHil, Util}

import org.scalatest.funsuite.AnyFunSuite

class PredicateExpansionTest extends AnyFunSuite:
  test("example 1") {
    val source = """
import mpi.MPI;

class Test {
    static void foo() {
        int rank = MPI.COMM_WORLD.Rank();
        int a = 0;
        if (rank == 0) {
            a = 1;
        } else {
            a = a + 1;
        }
    }
}
"""
    val expected = """func Test.foo(): void {
  var rank: int = mpi.Comm.Rank
  var a: int = 0
  var t~2: boolean = rank==0
  if (t~2) {
    assert t~2
    a = 1
  } else {
    var t~9: int = mpi.Comm.Rank
    var t~10: boolean = t~9==4
    if (t~10) {
      assert t~10
      a = a+1
    } else {
      var t~7: int = mpi.Comm.Rank
      var t~8: boolean = t~7==3
      if (t~8) {
        assert t~8
        a = a+1
      } else {
        var t~5: int = mpi.Comm.Rank
        var t~6: boolean = t~5==2
        if (t~6) {
          assert t~6
          a = a+1
        } else {
          var t~3: int = mpi.Comm.Rank
          var t~4: boolean = t~3==1
          if (t~4) {
            assert t~4
            a = a+1
          } else {
            a = a+1
          }
        }
      }
    }
  }
}"""
    val path = "examples/showcase/ForLoop.java"
    val hil = SourceToHil(JavaParser.parse(source))
    val expanded = PredicateExpansion(hil)
    val actual = PrettyPrint(expanded)
    println(actual)
    assert(actual === expected)
  }
