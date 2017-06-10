package tapl.component.rectype

import macros.Visitor
import tapl.component.typevar
import tapl.common._

@Visitor
trait TAlg[-F, T] extends typevar.TAlg[F, T] {
  def tyRec(x: String, t: F): T

  def apply(t: F): T
}
