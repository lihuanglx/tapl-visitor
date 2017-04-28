package tapl.common

import scalaz.Monad

trait EvalAuxiliary[A[-X, Y], M[_]] {
  implicit val m: Monad[M]

  val isVal: A[Exp[A], Boolean]

  val f: A[Exp[A], Exp[A]]
}
