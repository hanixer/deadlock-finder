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
  def time[R](block: => R): R =
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result

  def main(args: Array[String]): Unit =
    val pairs = generatePairs(10, 10000)
    for i <- 0 to 4 do
      val s1 = time {
        pairs.groupMap(_._1)(_._2).map((k, v) => (k, v.toSet))
      }
      val s2 = time {
        pairs.groupMapReduce(_._1)((_, v) => Set(v))((s, u) => s.union(u))
      }
      println(s1.size)
      println(s2.size)
      println("==============================")

  val r = scala.util.Random
  
  def generatePairs(n: Int, m: Int): Seq[(Int, Int)] =
    val kkk = for { 
        i <- 0 to n 
        j <- 0 to m        
    } yield (i, r.nextInt)
    kkk
      