package hir

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.HirToLil
import deadlockFinder.translation.SourceToHir
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
    val lil = HirToLil(SourceToHir(JavaParser.parse(source)))
    val func = lil.funcs(0)
    val cfg = CfgGraph(func)
    println(PrettyPrint(func))
    assert(cfg.getSuccs("bb1") === List("bb2"))
    assert(cfg.getPreds("bb1") === List())
    assert(cfg.getSuccs("bb2") === List("bb4", "bb3"))
  }
    