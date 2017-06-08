package tapl.component.varapp

import tapl.common._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def TmVar(x: String): Exp[A] = CVar[A](x)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  // todo
  override def TmVar(x: String): Boolean = true
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def TmVar(x: String): Exp[A] = if (x == this.x) e else CVar[A](x)
}