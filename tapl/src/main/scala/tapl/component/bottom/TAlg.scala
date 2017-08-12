package tapl.component.bottom

import macros.Language
import tapl.common._

@Language
trait TAlg[-F, T] {
  def tyBot(): T

  def apply(t: F): T
}
