package tapl.component.lambda

import tapl.common.{EvalAux, Exp, SubstAux}
import tapl.component.lambda.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAux[A, M] {
  override def TmAbs(x: String, e: Exp[A]): M[Exp[A]] = m.point(CAbs(x, e))
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmAbs(x: String, e: Exp[A]) = true
}

trait IsFuncVal[A[-R, _]] extends Query[Exp[A], Option[(String, Exp[A])]] {
  override val default: Option[(String, Exp[A])] = None

  override def TmAbs(x: String, e: Exp[A]) = Some((x, e))
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs[A](x, if (this.x == x) e else apply(e))
}
