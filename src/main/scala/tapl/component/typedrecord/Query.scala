package tapl.component.typedrecord

import tapl.common.Default
import tapl.component.record

trait Query[R, T] extends Alg[R, T] with record.Query[R, T]

trait TQuery[R, T] extends TAlg[R, T] with Default[T] {
  override def TyRecord(l: List[(String, R)]): T = default
}
