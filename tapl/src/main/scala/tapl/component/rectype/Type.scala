package tapl.component.rectype

import macros.Language
import tapl.common._

@Language
trait Type[-F, T] {
  def tyRec(x: String, t: F): T

  def apply(t: F): T
}