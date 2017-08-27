package inspect.component.floatstring

import inspect.common._
import inspect.component.floatstring.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmFloat(d: Double): Exp[A] = TmFloat[A, A](d)

  override def tmString(s: String): Exp[A] = TmString[A, A](s)

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
