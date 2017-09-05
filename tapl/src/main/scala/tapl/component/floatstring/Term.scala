package tapl.component.floatstring

import gems.Language
import tapl.common._

@Language
trait Term[-R, E] {
  def tmFloat(d: Double): E

  def tmTimes(e1: R, e2: R): E

  def tmString(s: String): E

  def apply(e: R): E
}
