package tapl.component.nat

import tapl.common.Exp
import tapl.common.Val._

import scalaz.Scalaz._
import scalaz._


trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Val]] {
  implicit val m: Monad[M]

  override def TmZero(): M[Val] = m.point(0)

  override def TmPred(e: Exp[A]): M[Val] = for {
    x <- visit(e) >>= intVal[M]
  } yield x - 1

  override def TmSucc(e: Exp[A]): M[Val] = for {
    x <- visit(e) >>= intVal[M]
  } yield x + 1

  override def TmIsZero(e: Exp[A]): M[Val] = for {
    x <- visit(e) >>= intVal[M]
  } yield x == 0
}
