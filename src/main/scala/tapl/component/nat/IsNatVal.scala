package tapl.component.nat

import tapl.common.Exp
import tapl.component.nat.Factory._

trait IsNatVal[A[-X, Y]] {
  // Int value and predecessor
  def isNatVal(e: Exp[A]): Option[(Int, Exp[A])] = e match {
    case CZero() => Some((0, e))
    case CSucc(x) => for {
      (i, _) <- isNatVal(x)
    } yield (i + 1, x)
    case _ => None
  }
}
