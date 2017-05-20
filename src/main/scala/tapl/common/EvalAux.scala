package tapl.common

import tapl.common.Util._

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

trait TyperAuxSub[A[-X, Y]] extends TyperAux[A] {
  val subtypeOf: A[Exp[A], Exp[A] => Boolean]

  def join(t1: Exp[A], t2: Exp[A]): Exp[A] =
    if (t1(subtypeOf)(t2)) t2 else if (t2(subtypeOf)(t1)) t1 else typeError()
}
