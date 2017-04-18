package tapl.component.varapp

import tapl.common.Exp
import tapl.common.Val._

import scalaz.Monad

trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Val]] {
  implicit val m: Monad[M]

  //todo: implement
  override def TmVar(x: String): M[Val] = ???

  override def TmApp(e1: Exp[A], e2: Exp[A]): M[Val] = ???
}
