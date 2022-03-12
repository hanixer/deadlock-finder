package deadlockFinder
package hil

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
      .sorted

  def runForFile(file: String, convert: Path => String): Unit =
    val expectedFile = file.replace(".java", ".out")
    val expected = Files.readString(Path.of(expectedFile))
    val actual = convert(Path.of(file))
    if !actual.equals(expected) then
      val actualFile = file.replace(".java", ".actual")
      Files.writeString(Path.of(actualFile), actual)
      assert(false, s"Differences in file $file")
