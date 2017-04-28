package tapl.component.record

import tapl.common.Default

trait Query[R, T] extends Alg[R, T] with Default[T] {
  override def TmRecord(l: List[(String, R)]): T = default

  override def TmProj(e: R, x: String): T = default
}
