package play.examples.lang2

import play.examples.Common._
import macros.Visitor
import play.examples._

@Visitor
trait Alg[-R, E, -F] extends lang1.Alg[R, E] {
  def abs(x: String, t: F, e: R): E
}
