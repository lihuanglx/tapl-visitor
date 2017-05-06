package tapl.component.nat

import tapl.common.Exp
import tapl.component.nat.Factory._

trait IsNatVal[A[-X, Y]] {
  def isNatVal(e: Exp[A]): Option[Int] = e match {
    case CZero() => Some(0)
    case CPred(x) => isNatVal(x).map(_ - 1)
    case CSucc(x) => isNatVal(x).map(_ + 1)
    case _ => None
  }
}
