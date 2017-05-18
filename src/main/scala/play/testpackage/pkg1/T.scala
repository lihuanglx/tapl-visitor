package play.testpackage.pkg1

trait E

case class C(x: Int) extends E

trait T {
  type AA = C
  val AA = C
}

object T extends T

object F {
  import T._
  val v = AA(3)
}