package convert.component.bool

import convert.common._
import convert.component.bool.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] with Term.Convert[A] {
  override def tmTrue(): Exp[A] = TmTrue[A, A]()

  override def tmFalse(): Exp[A] = TmFalse[A, A]()

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = {
    if (e1(isVal)) {
      convertBool(e1) match {
        case Some(x) => x(new Query[Exp[A], Exp[A]] {
          override def default: Exp[A] = typeError()

          override def tmTrue(): Exp[A] = e2

          override def tmFalse(): Exp[A] = e3

          override def apply(e: Exp[A]): Exp[A] = impossible
        })
        case _ => typeError()
      }
    } else {
      TmIf(apply(e1), e2, e3)
    }
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override def default: Boolean = false

  override def tmTrue(): Boolean = true

  override def tmFalse(): Boolean = true
}
