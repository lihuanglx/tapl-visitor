package tapl.component.typevar

import macros.Visitor
import tapl.common._

@Visitor
trait TAlg[-F, T] {
  def tyVar(x: String): T
}
