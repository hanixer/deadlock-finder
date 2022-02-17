package deadlockFinder
package hil

import org.scalatest.funsuite.AnyFunSuite
import deadlockFinder.translation.{HilToLil, LilToSsa, SourceToHil}
import deadlockFinder.JavaParser
import deadlockFinder.analysis.ConstantPropagation
import deadlockFinder.analysis.ConstantPropagation.{ConstantAbsVal}
import deadlockFinder.analysis.VarInfo

import deadlockFinder.cfg.CfgGraph
import deadlockFinder.common.PrettyPrint
import deadlockFinder.analysis.Use
import java.nio.file.Files
import java.nio.file.Path

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
    val lil = LilToSsa(HilToLil(SourceToHil(JavaParser.parse(source))))
    val func = lil.funcs.head
    val m = ConstantPropagation.getImmediateConstants(func)

    assert(m(VarInfo("x", Some(0))) === ConstantAbsVal.Constant(6))
    assert(m(VarInfo("y", Some(0))) === ConstantAbsVal.Constant(3))
    assert(m(VarInfo("b", Some(0))) === ConstantAbsVal.Undefined)
    assert(m(VarInfo("z", Some(1))) === ConstantAbsVal.Constant(5))
    assert(m(VarInfo("z", Some(0))) === ConstantAbsVal.Undefined)
  }
  test("example 2") {

    val source1 = """
public class Example8 {
    static void func1(int x, int y) {
        int z = 35;
        int w = z + 33;
        x = 398;
        if (x == 0) {
            x = w;
        } else {
            y = 5;
        }
        func2(x, y);
    }
    static void func2(int x, int y) {
    }
}
"""
    val java = JavaParser.parse(source1)
    val hil = SourceToHil(java)
    val lil = HilToLil(hil)
    val ssa = LilToSsa(lil)
    val func = ssa.funcs.head
    val consts = ConstantPropagation.computeConstants(func)
    assert(consts(VarInfo("x", Some(2))) === ConstantAbsVal.NotConstant)
    assert(consts(VarInfo("x", Some(1))) === ConstantAbsVal.Constant(68))
  }
