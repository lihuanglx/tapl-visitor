package tapl.component.nat

import tapl.common.Exp
import tapl.common.Value._

import scalaz.Monad

trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Value]] {
  implicit val m: Monad[M]

  override def TmZero(): M[Value] = m.point(0)

  // todo: "for" notation
  override def TmPred(e: Exp[A]): M[Value] =
    m.bind(visit(e))(intVal andThen (x => m.point(x - 1)))

  override def TmSucc(e: Exp[A]): M[Value] =
    m.bind(visit(e))(intVal andThen (x => m.point(x + 1)))

  override def TmIsZero(e: Exp[A]): M[Value] =
    m.bind(visit(e))(intVal andThen (x => m.point(x == 0)))
}
