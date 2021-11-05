import scala.collection.mutable.ListBuffer
object Main2:
  def main(args: Array[String]): Unit =
    val a = ListBuffer(1)
    val b = a.toList
    println(a)
    println(b)
    println("==================")
    a.clear
    println(a)
    println(b)
