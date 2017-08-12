package play.examples.lang1

import play.examples.Common._
import macros.Language
import play.examples._

@Language
trait Alg[-N, E] extends lang0.Alg[N, E] {
  def vr(x: String): E
}
