package tapl.common

import tapl.common.Util.Type

trait EvalAux[A[-X, Y]] {
  val isVal: A[Exp[A], Boolean]
}

trait EvalSubst[A[-R, _]] extends EvalAux[A] {
  val subst: (String, Exp[A]) => A[Exp[A], Exp[A]]
}

trait SubstAux[A[-R, _]] {
  val x: String
  val e: Exp[A]
}

trait TyperAux[A[-X, Y]] {
  val tEquals: A[Exp[A], Exp[A] => Boolean]

  implicit def constType(t: Exp[A]): Type[A] = _ => t
}
