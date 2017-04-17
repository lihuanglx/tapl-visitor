package tapl.component.nat

import tapl.common.Exp
import tapl.common.Value._

import scalaz.Monad

trait Eval[A[-R, _], M[_] <: Monad[M]] extends Alg[Exp[A], M[Value]] {
  implicit def M: Monad[M]

  override def TmZero(): M[Value] = M.point(0)

  // todo: "for" notation
  override def TmPred(e: Exp[A]): M[Value] =
    M.bind(visit(e))(intVal andThen (x => M.point(x - 1)))

  override def TmSucc(e: Exp[A]): M[Value] =
    M.bind(visit(e))(intVal andThen (x => M.point(x + 1)))

  override def TmIsZero(e: Exp[A]): M[Value] =
    M.bind(visit(e))(intVal andThen (x => M.point(x == 0)))
}
