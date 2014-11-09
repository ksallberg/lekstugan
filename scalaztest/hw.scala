import scalaz._
import std.option._, std.list._

object Hi {
    def main(args: Array[String]) =
        println("Hi!")
        val x = Apply[Option].apply2(some(1), some(2))((a, b) => a + b)
        val y = Traverse[List].traverse(List(1, 2, 3))(i => some(i))
        println("Testing x:")
        println(x)
        println("Testing y:")
        println(y)
}
