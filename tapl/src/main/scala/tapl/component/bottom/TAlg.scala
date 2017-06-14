package tapl.component.bottom

import macros.Visitor
import tapl.common._

@Visitor
trait TAlg[-F, T] {
  def tyBot(): T

  def apply(t: F): T
}
