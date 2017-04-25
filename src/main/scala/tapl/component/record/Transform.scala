package tapl.component.record

import tapl.common.Exp

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  val f: Factory[A]

  override def TmRecord(l: List[(String, Exp[A])]): Exp[A] = f.TmRecord(l.map {case (x, e) => (x, apply(e))})

  override def TmProj(e: Exp[A], x: String): Exp[A] = f.TmProj(apply(e), x)
}
