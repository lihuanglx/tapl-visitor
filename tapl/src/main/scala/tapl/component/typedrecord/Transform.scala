package tapl.component.typedrecord

import tapl.common.Exp
import tapl.component.record

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with record.Transform[A]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]] {
  override def TyRecord(l: List[(String, Exp[A])]): Exp[A] =
    CTyRecord[A](l.map { case (n, t) => (n, apply(t)) })
}
