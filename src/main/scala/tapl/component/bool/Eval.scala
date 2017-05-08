package tapl.component.bool

import tapl.common.Util.typeError
import tapl.common.{EvalAux, Exp}
import tapl.component.bool.Factory._

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAux[A, M] {
  override def TmTrue(): M[Exp[A]] = m.point(CTrue[A]())

  override def TmFalse(): M[Exp[A]] = m.point(CFalse[A]())

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Exp[A]] = {
    if (e1(isVal)) {
      val r = e1 match {
        case CTrue() => e2
        case CFalse() => e3
        case _ => typeError()
      }
      m.point(r)
    } else for {
      _e1 <- apply(e1)
    } yield CIf(_e1, e2, e3)
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmTrue(): Boolean = true

  override def TmFalse(): Boolean = true
}
