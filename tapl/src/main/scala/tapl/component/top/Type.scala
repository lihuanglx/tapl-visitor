package tapl.component.top

import macros.Language
import tapl.common._

@Language
trait Type[-F, T] {
  def tyTop(): T

  def apply(t: F): T
}
