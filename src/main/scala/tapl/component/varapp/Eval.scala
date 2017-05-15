package tapl.component.varapp

import tapl.common.{EvalSubst, Exp, SubstAux, Util}
import tapl.component.varapp.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with EvalSubst[A] {
  val isFuncVal: A[Exp[A], Option[(String, Exp[A])]]

  override def TmVar(x: String): Exp[A] = CVar[A](x)

  override def TmApp(e1: Exp[A], e2: Exp[A]): Exp[A] = {
    if (e1(isVal)) {
      if (e2(isVal)) {
        val (x, body) = e1(isFuncVal).getOrElse(Util.typeError())
        subst(x, e2)(body)
      } else {
        CApp(e1, apply(e2))
      }
    } else {
      CApp(apply(e1), e2)
    }
  }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  // todo
  override def TmVar(x: String): Boolean = true
}

trait Subst[A[-X, Y] <: Alg[X, Y]] extends Transform[A] with SubstAux[A] {
  override def TmVar(x: String): Exp[A] = if (x == this.x) e else CVar[A](x)
}