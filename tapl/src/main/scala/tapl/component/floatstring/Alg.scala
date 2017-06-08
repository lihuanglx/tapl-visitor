package tapl.component.floatstring

import macros.Visitor
import tapl.common._

@Visitor
trait Alg[-R, E] {
  def tmFloat(d: Double): E

  def tmTimes(e1: R, e2: R): E

  def tmString(s: String): E

  def apply(e: R): E
}
