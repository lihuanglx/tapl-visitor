package tapl.component.record

import tapl.common.Exp
import tapl.component.record.Factory._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmRecord(l: List[(String, Exp[A])]): Exp[A] = CRecord(l.map { case (x, e) => (x, apply(e)) })

  override def TmProj(e: Exp[A], x: String): Exp[A] = CProj(apply(e), x)
}
