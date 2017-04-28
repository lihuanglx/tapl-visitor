package tapl.component.floatstring

import tapl.common.{EvalAuxiliary, Exp}

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  override def TmFloat(d: Double): M[Exp[A]] = m.point(f.TmFloat(d))

  override def TmString(s: String): M[Exp[A]] = m.point(f.TmString(s))

  override def TmTimes(e1: Exp[A], e2: Exp[A]): M[Exp[A]] =
    if (e1(isVal)) {
      if (e2(isVal)) {
        // todo
        ???
      } else for {
        _e2 <- apply(e2)
      } yield f.TmTimes(e1, _e2)
    } else for {
      _e1 <- apply(e1)
    } yield f.TmTimes(_e1, e2)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmString(s: String): Boolean = true

  override def TmFloat(d: Double): Boolean = true
}