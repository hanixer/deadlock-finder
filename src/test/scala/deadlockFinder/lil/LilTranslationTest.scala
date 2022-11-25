package deadlockFinder
package lil

import common.PrettyPrint
import translation.{FileHelper, HilToLil, SourceToHil}

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class LilTranslationTest extends AnyFunSuite {
  def toLil(path: Path): String =
    PrettyPrint(HilToLil(SourceToHil(JavaParser.parseFile(path))))
  val javaFiles = FileHelper.getJavaFiles("test/lil/")
  // TODO: refresh tests
//  for f <- javaFiles do
//    test(f) {
//      FileHelper.runForFile(f, toLil)
//    }
}
