package tapl.component.typedrecord

import macros.Language
import tapl.common._
import tapl.component.record

@Language
trait Alg[-R, E] extends record.Alg[R, E]

@Language
trait TAlg[-F, T] {
  def tyRecord(l: List[(String, F)]): T

  def apply(t: F): T
}
