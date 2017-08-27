package inspect.component.bool

import inspect.common._
import inspect.component.bool.Term._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] with Term.Inspect[A] {
  override def tmTrue(): Exp[A] = TmTrue[A, A]()

  override def tmFalse(): Exp[A] = TmFalse[A, A]()

  override def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = {
    if (e1(isVal)) {
      inspectBool(e1) match {
        case Some(x) => x(new Query[Exp[A], Exp[A]] {
          override val default: Exp[A] = typeError()

          override def tmTrue(): Exp[A] = e2

          override def tmFalse(): Exp[A] = e3

          override def apply(e: Exp[A]): Exp[A] = sys.error("impossible")
        })
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
