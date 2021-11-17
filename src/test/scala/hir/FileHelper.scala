package deadlockFinder
package hir

import org.scalatest.Assertions.convertToEqualizer

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

object FileHelper:
  def getJavaFiles(dir: String): List[String] =
    Files
      .list(Path.of(dir))
      .iterator()
      .asScala
      .toList
      .map(p => p.toAbsolutePath.toString)
      .filter(p => p.endsWith(".java"))

  def runForFile(file: String, convert: Path => String): Unit =
    val expectedFile = file.replace(".java", ".out")
    val expected = Files.readString(Path.of(expectedFile))
    val actual = convert(Path.of(file))
    assert(actual.equals(expected), s"Differences in file $file")
