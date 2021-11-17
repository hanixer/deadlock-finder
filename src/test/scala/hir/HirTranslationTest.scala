package deadlockFinder
package hir

import translation.SourceToHir
import common.PrettyPrint
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Path}

class HirTranslationTest extends AnyFunSuite {
  def toHir(path: Path): String =
    PrettyPrint(SourceToHir(JavaParser.parseFile(path)))

  test("All files") {
    for f <- FileHelper.getJavaFiles("test/hir/") do
      FileHelper.runForFile(f, toHir)
  }
}
