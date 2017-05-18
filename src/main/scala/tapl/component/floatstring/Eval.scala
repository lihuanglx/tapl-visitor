package tapl.component.floatstring

import tapl.common.{EvalAux, Exp}

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with EvalAux[A] {
  override def TmFloat(d: Double): Exp[A] = CFloat[A](d)

  override def TmString(s: String): Exp[A] = CString[A](s)

  override def TmTimes(e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      if (e2(isVal)) {
        // todo
        ???
      } else {
        CTimes(e1, apply(e2))
      }
    } else {
      CTimes(apply(e1), e2)
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmString(s: String): Boolean = true

  override def TmFloat(d: Double): Boolean = true
}