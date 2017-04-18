package tapl.component.bool

import tapl.common.Exp
import tapl.common.Value._

import scalaz.Monad

trait Eval[A[-R, _], M[_]] extends Alg[Exp[A], M[Value]] {
  implicit val m: Monad[M]

  override def TmTrue(): M[Value] = m.point(true)

  override def TmFalse(): M[Value] = m.point(false)

  // todo: "for" notation
  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): M[Value] =
    m.bind(visit(e1))(boolVal andThen (if (_) visit(e2) else visit(e3)))
}
