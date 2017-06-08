package tapl.component.typedrecord

import macros.Visitor
import tapl.common._
import tapl.component.record

@Visitor
trait Alg[-R, E] extends record.Alg[R, E]

@Visitor
trait TAlg[-F, T] {
  def tyRecord(l: List[(String, F)]): T

  def apply(t: F): T
}
