package tapl.component.bool

import tapl.common._
import tapl.component.bool.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmTrue(): Exp[A] = TmTrue[A]()

  override def tmFalse(): Exp[A] = TmFalse[A]()

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = {
    if (e1(isVal)) {
      e1 match {
        case TmTrue() => e2
        case TmFalse() => e3
        case _ => typeError()
      }
    } else {
      TmIf(apply(e1), e2, e3)
    }
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def tmTrue(): Boolean = true

  override def tmFalse(): Boolean = true
}
