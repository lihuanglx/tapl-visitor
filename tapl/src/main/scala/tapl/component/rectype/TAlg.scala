package tapl.component.rectype

import macros.Language
import tapl.common._

@Language
trait TAlg[-F, T] {
  def tyRec(x: String, t: F): T

  def apply(t: F): T
}
