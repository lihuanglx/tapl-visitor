package play.examples.lang0

import macros.Visitor
import play.examples.Common._

@Visitor("debug")
trait Alg[-R, E] {
  def lit(x: Int): E

  def add(e1: R, e2: R): E

  def apply(e: R): E
}
