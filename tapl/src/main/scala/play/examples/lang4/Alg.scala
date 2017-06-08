package play.examples.lang4

import play.examples.Common._
import macros.Visitor

@Visitor
trait Alg[-R, E, -F] extends play.examples.lang2.Alg[R, E, F] {
  def anno(e: R, t: F): E
}
