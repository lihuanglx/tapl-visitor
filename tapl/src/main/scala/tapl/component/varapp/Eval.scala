package tapl.component.varapp

import tapl.common._
import tapl.component.varapp.Alg._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {
  override def tmVar(x: String): Exp[A] = TmVar[A](x)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  // todo
  override def tmVar(x: String): Boolean = true
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def tmVar(x: String): Exp[A] = if (x == this.x) e else TmVar[A](x)
}
