package tapl.language.arith

import tapl.common.Exp
import tapl.component._


trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends
  Alg[Exp[A], M[Exp[A]]] with bool.Eval[A, M] with nat.Eval[A, M]


trait EvalM[M[_]] extends Eval[Alg, M] {
  override def apply(e: Exp[Alg]): M[Exp[Alg]] = e(this)

  override val f = Factory

  override val isVal: Alg[Exp[Alg], Boolean] = IsValImpl

  override val isNumVal: Alg[Exp[Alg], Option[Int]] = IsNumValImpl

  override val isBoolVal: Alg[Exp[Alg], Option[Boolean]] = IsBoolValImpl
}


trait IsVal[A[-R, _]] extends Alg[Exp[A], Boolean] with bool.IsVal[A] with nat.IsVal[A]

object IsValImpl extends IsVal[Alg] {
  override def apply(e: Exp[Alg]): Boolean = e(this)
}


trait IsNumVal[A[-R, _]] extends Query[A, Option[Int]] with nat.IsNumVal[A]

object IsNumValImpl extends IsNumVal[Alg] {
  override def apply(e: Exp[Alg]): Option[Int] = e(this)
}


trait IsBoolVal[A[-R, _]] extends Query[A, Option[Boolean]] with bool.IsBoolVal[A]

object IsBoolValImpl extends IsBoolVal[Alg] {
  override def apply(e: Exp[Alg]): Option[Boolean] = e(this)
}