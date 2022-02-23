package deadlockFinder
package analysis

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HilToLil, LilToSsa, SourceToHil}
import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.JavaParser
import deadlockFinder.analysis.{UsesAndDefs, VarInfo}
import deadlockFinder.hil.IntLiteral

class DefsTest extends AnyFunSuite:
  test("example 1") {
    val source = """
public class Example1 {
    static void func1() {
        int x = 6;
        int y = x + 3;
        x = x + y;
        y = x;
    }
}
"""
    val hil = SourceToHil(JavaParser.parse(source))
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val usesAndDefs = UsesAndDefs(func)
    val e1 = usesAndDefs.getDefiningExpr(VarInfo("x", Some(0)))
    assert(e1.isDefined)
    val e1e = e1.get
    assert(e1e.isInstanceOf[IntLiteral], "x should be literal")
    assert(e1e.asInstanceOf[IntLiteral].n === 6, "x should be literal")
  }
    