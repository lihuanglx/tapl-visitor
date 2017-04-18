package tapl.component.bool

import tapl.common.Exp
import tapl.common.Val._

import scalaz.Scalaz._
import scalaz._


trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Val]] {
  implicit val m: Monad[M]

  override def TmTrue(): M[Val] = m.point(true)

  override def TmFalse(): M[Val] = m.point(false)

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Val] = for {
    x <- visit(e1) >>= boolVal[M]
    r <- if (x) visit(e2) else visit(e3)
  } yield r
}
