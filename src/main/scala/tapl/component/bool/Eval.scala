package tapl.component.bool

import tapl.common.{EvalAuxiliary, Exp}
import tapl.common.Util.typeError


trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  val f: Alg[Exp[A], Exp[A]]

  val isBoolVal: A[Exp[A], Option[Boolean]]

  override def TmTrue(): M[Exp[A]] = m.point(f.TmTrue())

  override def TmFalse(): M[Exp[A]] = m.point(f.TmFalse())

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Exp[A]] = {
    if (e1(isVal)) {
      val r = e1(isBoolVal) match {
        case Some(b) => if (b) e2 else e3
        case _ => typeError()
      }
      m.point(r)
    } else {
      m.bind(apply(e1))(x => m.point(f.TmIf(x, e2, e3)))
    }
  }
}


trait IsVal[A[-R, _]] extends Query[A, Boolean] {
  override val default: Boolean = false

  override def TmTrue(): Boolean = true

  override def TmFalse(): Boolean = true
}


trait IsBoolVal[A[-R, _]] extends Query[A, Option[Boolean]] {
  override val default: Option[Boolean] = None

  override def TmTrue(): Option[Boolean] = Some(true)

  override def TmFalse(): Option[Boolean] = Some(false)
}
