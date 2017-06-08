package tapl.component.floatstring

import tapl.common._
import tapl.component.floatstring.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmFloat(d: Double): Exp[A] = TmFloat[A](d)

  override def tmString(s: String): Exp[A] = TmString[A](s)

  override def tmTimes(e1: Exp[A], e2: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      if (e2(isVal)) {
        // todo
        ???
      } else {
        TmTimes(e1, apply(e2))
      }
    } else {
      TmTimes(apply(e1), e2)
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def tmString(s: String): Boolean = true

  override def tmFloat(d: Double): Boolean = true
}
