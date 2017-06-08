package tapl.component.top

import macros.Visitor
import tapl.common._

@Visitor
trait TAlg[-F, T] {
  def tyTop(): T

  def apply(t: F): T
}
