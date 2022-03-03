package deadlockFinder
package cfg

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HilToLil
import deadlockFinder.translation.SourceToHil
import deadlockFinder.JavaParser
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint

class ImmediateDominatorsTest extends AnyFunSuite:
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
    assert(cfg.getSuccs("bb1") === List("bb2"))
    assert(cfg.getPreds("bb1") === List("entry"))
    assert(cfg.getSuccs("bb2") === List("bb4", "bb3"))
    assert(cfg.getPreds("bb2") === List("bb1", "bb8"))
  }
    