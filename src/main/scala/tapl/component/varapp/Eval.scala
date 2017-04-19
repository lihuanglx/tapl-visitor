package tapl.component.varapp

import tapl.common.Exp

import scalaz._


trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Exp[A]]] {
  implicit val m: Monad[M]

  val subst: (String, Exp[A]) => Alg[Exp[A], Exp[A]]

  //todo: implement
  override def TmVar(x: String): M[Exp[A]] = ???

  override def TmApp(e1: Exp[A], e2: Exp[A]): M[Exp[A]] = ???
}
