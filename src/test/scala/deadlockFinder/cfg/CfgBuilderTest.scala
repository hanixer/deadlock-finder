package deadlockFinder
package cfg

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HilToLil
import deadlockFinder.translation.SourceToHil
import deadlockFinder.JavaParser
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint

class CfgBuilderTest extends AnyFunSuite:
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
            while (x > 4) {
                if (y == 23) 
                    continue;
                x = x - 1;
            }
        }
    }
}
"""
    val lil = HilToLil(SourceToHil(JavaParser.parse(source)))
    val func = lil.funcs(0)
    val cfg = CfgGraph(func)
    println(PrettyPrint(func))
    assert(cfg.successors("bb1") === List("bb2"))
    assert(cfg.predecessors("bb1") === List("entry"))
    assert(cfg.successors("bb2") === List("bb3", "end"))
    assert(cfg.predecessors("bb2") === List("bb1", "bb4"))
  }
    