package deadlockFinder

import java.nio.file.{Files, Path}
import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

trait Iner:
  ???

object Iner:
  case class Kkk(n: Int) extends Iner
  case class KKkere(s: String) extends Iner

object Main2:
  def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def main(args: Array[String]): Unit =
    for i <- 0 to 4 do
      val l = time {recurByList(1, 3)}
      val s = time {recurBySet(1, 3)}
      val jjj = l.toSet.toList
      println(s"list size: ${l.length}, set size: ${s.size}")

  def recurByList(curr: Int, depth: Int): List[Int] =
    if depth == 0 then List(curr)
    else curr :: recurByList(curr * 2, depth - 1) ++ recurByList(curr * 20 + 1, depth - 1)

  def recurBySet(curr: Int, depth: Int): Set[Int] =
    if depth == 0 then Set(curr)
    else recurBySet(curr * 2, depth - 1).union(recurBySet(curr * 20 + 1, depth - 1)).incl(curr)
