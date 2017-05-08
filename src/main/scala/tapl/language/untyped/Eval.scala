package tapl.language.untyped

import tapl.common.Exp
import tapl.component._
import tapl.language.untyped.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with varapp.Eval[A, M] {
  override def TmAbs(x: String, e: Exp[A]): M[Exp[A]] = m.point(CAbs(x, e))
}

trait EvalM[M[_]] extends Eval[Alg, M] with Impl[M[Exp[Alg]]] {
  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override val isFuncVal: Alg[Exp[Alg], Option[(String, Exp[Alg])]] = IsFuncValImpl

  override val subst: (String, Exp[Alg]) => Alg[Exp[Alg], Exp[Alg]] = (x, e) => new SubstImpl(x, e)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] with varapp.IsVal[A] {
  override def TmAbs(x: String, e: Exp[A]) = true
}

object IsValImpl extends IsVal[Alg] with Impl[Boolean]

trait IsFuncVal[A[-R, _]] extends Query[Exp[A], Option[(String, Exp[A])]] {
  override val default: Option[(String, Exp[A])] = None

  override def TmAbs(x: String, e: Exp[A]) = Some((x, e))
}

object IsFuncValImpl extends IsFuncVal[Alg] with Impl[Option[(String, Exp[Alg])]]

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with varapp.Subst[A] {
  override def TmAbs(x: String, e: Exp[A]): Exp[A] = CAbs[A](x, if (this.x == x) e else apply(e))
}

class SubstImpl(_x: String, _e: Exp[Alg]) extends Subst[Alg] with Impl[Exp[Alg]] {
  override val x: String = _x
  override val e: Exp[Alg] = _e
}