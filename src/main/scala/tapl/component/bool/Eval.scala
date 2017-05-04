package tapl.component.bool

import tapl.common.{EvalAuxiliary, Exp}
import tapl.common.Util.typeError

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  def matcher[E]: Matcher[A, E]

  override def TmTrue(): M[Exp[A]] = m.point(f.TmTrue())

  override def TmFalse(): M[Exp[A]] = m.point(f.TmFalse())

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Exp[A]] = {
    if (e1(isVal)) {
      //val b = e1(isBoolVal).getOrElse(typeError())
      //m.point(if (b) e2 else e3)
      val r = matcher.CaseTrue(e2).CaseFalse(e3).CaseDefault(typeError()).apply(e1)
      m.point(r)
    } else for {
      _e1 <- apply(e1)
    } yield f.TmIf(_e1, e2, e3)
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmTrue(): Boolean = true

  override def TmFalse(): Boolean = true
}
