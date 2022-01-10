package module2

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object implicits {

  // implicit conversions

  object implicit_conversions {

    /** Расширить возможности типа String, методом trimToOption, который возвращает Option[String]
      * если строка пустая или null, то None
      * если нет, то Some от строки со всеми удаленными начальными и конечными пробелами
      */

    lazy  val str: String = "ab"

      str.trimToOption  // "" -> None
      str.trimToOption  // "sasa" -> Some("sasa")
      str.trimToOption  // "  " -> None
      str.trimToOption  // null -> None

      "bvfbgg".trimToOption


      class StringOps(str: String){
        def trimToOption: Option[String] = 
          Option(str).map(_.trim).filter(_.nonEmpty)

          def length: Int = ???
      }

    implicit def strToStringsOps(str: String): StringOps = new StringOps(str) // String -> StringOps




    // implicit conversions ОПАСНЫ
    implicit def strToInt(str: String): Int = Integer.parseInt(str) // String -> Int

    //"fooo" / 42


    implicit val seq = Seq("a", "b", "c")  // Int => String
    
    
    def log(str: String) = println(str)

    val res = log(42)


  }

  object implicit_scopes {

    trait Printable

    trait Printer[T] extends Printable {
      def print(v: T): Unit
    }

    object Printable {
      // implicit val v: Printer[Bar] = new Printer[Bar] {
      //   override def print(v: Bar): Unit = println(s"Implicit from companion object Printable + $v")
      // }
    }

    // companion object Printer
    object Printer {
      // implicit val v: Printer[Bar] = new Printer[Bar] {
      //   override def print(v: Bar): Unit = println(s"Implicit from companion object Printer + $v")
      // }
    }

    case class Bar()

    // companion object Bar
    object Bar {
        implicit val v: Printer[Bar] = new Printer[Bar] {
          override def print(v: Bar): Unit = println(s"Implcit from companion object Bar + $v")
        }
    }

    // some arbitrary object
    object wildcardImplicits {
      implicit val v: Printer[Bar] = new Printer[Bar] {
        override def print(v: Bar): Unit = println(s"Implcit from wildcard import + $v")
      }
    }

    def foo(b: Bar)(implicit m: Printer[Bar]) = m.print(b)

    // implicit val v1 = new Printer[Bar]{
    //   def print(v: Bar): Unit = println(s"Implcit from local val + $v")
    // }

    //import wildcardImplicits._

    val result = foo(Bar())

    implicit var x: Int = ???

    // 1. Local scope | package object 
    //

  }


  object implicits_all_together{
    

    abstract class Ordering[T](val a: T){
      def less(b: T): Boolean
    }

    implicit def intToOrderingInt(i: Int): Ordering[Int] = new Ordering[Int](i) {
      def less(b: Int): Boolean = a < b
    }
    
    def max[A](a: A, b: A)(implicit ordering: A => Ordering[A]): A = 
      if(ordering(a).less(b)) b else a

    def max2[A <% Ordering[A]](a: A, b: A): A = {
      val ordering = implicitly[A => Ordering[A]]
      if(ordering(a).less(b)) b else a
    }

  
    lazy val r1 = println(max2(5, 10))
    lazy val r2 = println(max2(10, 15))

  }

}
