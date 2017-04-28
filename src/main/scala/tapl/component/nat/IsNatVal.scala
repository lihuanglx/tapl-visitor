package tapl.component.nat

import tapl.common.Exp

trait IsNatVal[A[-X, Y]] {
  def matcher[E]: Matcher[A, E]

  def isNatVal(e: Exp[A]): Option[Int] = matcher[Option[Int]].
    CaseZero(Some(0)).
    CasePred(isNatVal(_).map(_ - 1)).
    CaseSucc(isNatVal(_).map(_ + 1)).
    CaseDefault(None).
    apply(e)
}
