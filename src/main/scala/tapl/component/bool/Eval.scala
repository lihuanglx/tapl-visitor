package tapl.component.bool

import tapl.common._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with IIsVal[A] {
  override def TmTrue(): Exp[A] = CTrue[A]()

  override def TmFalse(): Exp[A] = CFalse[A]()

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = {
    if (e1(isVal)) {
      e1 match {
        case CTrue() => e2
        case CFalse() => e3
        case _ => typeError()
      }
    } else {
      CIf(apply(e1), e2, e3)
    }
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmTrue(): Boolean = true

  override def TmFalse(): Boolean = true
}
