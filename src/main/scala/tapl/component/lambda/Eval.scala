package tapl.component.lambda

import tapl.common.{EvalAuxiliary, Exp}

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  val f: A[Exp[A], Exp[A]]

  override def TmAbs(x: String, e: Exp[A]): M[Exp[A]] = m.point(f.TmAbs(x, e))
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmAbs(x: String, e: Exp[A]) = true
}

trait IsFuncVal[A[-R, _]] extends Query[Exp[A], Option[(String, Exp[A])]] {
  override val default: Option[(String, Exp[A])] = None

  override def TmAbs(x: String, e: Exp[A]) = Some((x, e))
}
