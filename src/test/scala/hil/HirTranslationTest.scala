package deadlockFinder
package hil

import translation.SourceToHil
import common.PrettyPrint
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class HilTranslationTest extends AnyFunSuite {
  def toHil(path: Path): String =
    PrettyPrint(SourceToHil(JavaParser.parseFile(path)))

  test("All files") {
    for f <- FileHelper.getJavaFiles("test/hil/") do
      FileHelper.runForFile(f, toHil)
  }
}
