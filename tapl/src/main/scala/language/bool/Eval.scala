package language
package bool

import gems._
import Term.Factory._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]]
  with IIsVal[A] with Term.Convert[A] {

  def tmTrue(): Exp[A] = TmTrue[A, A]()

  def tmFalse(): Exp[A] = TmFalse[A, A]()

  def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] =
    if (e1(isVal)) {
      val c = convertBool(e1).getOrElse(cnvFailed)
      c(new Term[Exp[A], Exp[A]] {
        def tmTrue(): Exp[A] = e2

        def tmFalse(): Exp[A] = e3

        def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = runtimeError

        def apply(e: Exp[A]): Exp[A] = impossible
      })
    }
    else
      TmIf[A, A](apply(e1), e2, e3)
}

trait IsVal[A[-R, _]] extends Term.Query[Exp[A], Boolean] {
  override def default: Boolean = false

  override def tmTrue(): Boolean = true

  override def tmFalse(): Boolean = true
}

trait Print[A[-R, _]] extends Term[Exp[A], String] {
  def tmTrue(): String = "true"

  def tmFalse(): String = "false"

  def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): String =
    s"if (${apply(e1)}) then (${apply(e2)}) else (${apply(e3)})"
}

trait PrintT[A[-R, _]] extends Type[Exp[A], String] {
  def tyBool(): String = "Bool"
}
