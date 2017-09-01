package tapl.component.nat

import tapl.common.Exp
import tapl.component.nat.Term._

trait IsNatVal[A[-X, Y]] {
  def isNatVal(e: Exp[A]): Option[Int] = e match {
    case TmZero() => Some(0)
    case TmSucc(x) => isNatVal(x).map(_ + 1)
    case _ => None
  }
}
