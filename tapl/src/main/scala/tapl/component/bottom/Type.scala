package tapl.component.bottom

import macros.Language
import tapl.common._

@Language
trait Type[-F, T] {
  def tyBot(): T

  def apply(t: F): T
}