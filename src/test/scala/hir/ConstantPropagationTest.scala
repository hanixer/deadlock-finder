package deadlockFinder
package hir

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HirToLil, LilToSsa, SourceToHir}
import deadlockFinder.JavaParser
import deadlockFinder.analysis.ConstantPropagation
import deadlockFinder.analysis.ConstantPropagation.{ConstantAbsVal}
import deadlockFinder.analysis.VarInfo

import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.analysis.Use

class ConstantPropagationTest extends AnyFunSuite:
  test("example 1") {

    val source = """
public class Example1 {
    static void func1() {
        int x = 6;
        int y = 3;
        int b = x + y;
        while (x < 100) {
            int z = 5;
            z = z + x;
        }
    }
}
"""
    val lil = LilToSsa(HirToLil(SourceToHir(JavaParser.parse(source))))
    val func = lil.funcs.head
    val m = ConstantPropagation.getImmediateConstants(func)

    assert(m(VarInfo("x", Some(0))) === ConstantAbsVal.Constant(6))
    assert(m(VarInfo("y", Some(0))) === ConstantAbsVal.Constant(3))
    assert(m(VarInfo("b", Some(0))) === ConstantAbsVal.Undefined)
    assert(m(VarInfo("z", Some(1))) === ConstantAbsVal.Constant(5))
    assert(m(VarInfo("z", Some(0))) === ConstantAbsVal.Undefined)
  }
