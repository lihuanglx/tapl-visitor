package play.examples.lang1

import play.examples.Common._
import macros.Visitor
import play.examples._

@Visitor
trait Alg[-N, E] extends lang0.Alg[N, E] {
  def vr(x: String): E
}

object Test {
  def main(args: Array[String]): Unit = {

  }
}
