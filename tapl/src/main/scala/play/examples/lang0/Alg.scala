package play.examples.lang0

import macros.Language
import play.examples.Common._

@Language
trait Alg[-R, E] {
  def lit(x: Int): E

  def add(e1: R, e2: R): E

  def apply(e: R): E
}
