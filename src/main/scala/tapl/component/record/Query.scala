package tapl.component.record

import tapl.common.{Default, Exp}

trait Query[A[-R, _], T] extends Alg[Exp[A], T] with Default[T] {
  override def TmRecord(l: List[(String, Exp[A])]): T = default

  override def TmProj(e: Exp[A], x: String): T = default
}
