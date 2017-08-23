package tapl.component.typedrecord

import macros.Language
import tapl.common._
import tapl.component.record

@Language
trait Term[-R, E] extends record.Term[R, E]

@Language
trait Type[-F, T] {
  def tyRecord(l: List[(String, F)]): T

  def apply(t: F): T
}
