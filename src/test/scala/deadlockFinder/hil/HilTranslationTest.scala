package deadlockFinder
package hil

import translation.{FileHelper, SourceToHil}
import common.PrettyPrint

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class HilTranslationTest extends AnyFunSuite {
  def toHil(path: Path): String =
    PrettyPrint(SourceToHil(JavaParser.parseFile(path)))
  val javaFiles = FileHelper.getJavaFiles("test/hil/")
  // TODO: refresh tests
//  for f <- javaFiles do
//    test(f) {
//      FileHelper.runForFile(f, toHil)
//    }
}
