package deadlockFinder
package translation

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HilToLil
import deadlockFinder.translation.SourceToHil
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser

class AssertInsertionTest extends AnyFunSuite:
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
    val expected = """func Example2.func1(): void {
  var rank: int = mpi.Comm.Rank(mpi.MPI.COMM_WORLD)
  var x: int = 6
  var t~1: boolean = rank==0
  if (t~1) {
    assert t~1
    x = 100
  } else {
    assert !t~1
    var t~2: boolean = rank==1
    if (t~2) {
      assert t~2
      x = 200
    } else {
      assert !t~2
    }
  }
}"""
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val func = hil.funcs.head
    val pretty = PrettyPrint(func)
    println(pretty)
    assert(pretty === expected)
  }
  test("example 1") {
    val source = """
public class Example1 {
    static void func1() {
        int x = 6;
        int y = 3;
        while (x < 100) {
            x = x + 1;
            if (y == 50) 
                break;
            if (y == 100)
                y = 10;
            else
                y = 20;
        }
    }
}
"""
    val expected = """func Example1.func1(): void {
  var x: int = 6
  var y: int = 3
  loop {
    var t~1: boolean = x<100
    var t~2: boolean = !t~1
    if (t~2) {
      assert t~2
      break
    } else {
      assert !t~2
    }
    x = x+1
    var t~3: boolean = y==50
    if (t~3) {
      assert t~3
      break
    } else {
      assert !t~3
    }
    var t~4: boolean = y==100
    if (t~4) {
      assert t~4
      y = 10
    } else {
      assert !t~4
      y = 20
    }
  }
}"""
    val hil = AssertInsertion(SourceToHil(JavaParser.parse(source)))
    val func = hil.funcs.head
    val pretty = PrettyPrint(func)
    println(pretty)
    assert(pretty === expected)
  }
    