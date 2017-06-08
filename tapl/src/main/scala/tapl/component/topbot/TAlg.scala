package tapl.component.topbot

import macros.Visitor
import tapl.common._
import tapl.component.top

@Visitor
trait TAlg[-F, T] extends top.TAlg[F, T] {
  def tyBot(): T
}
