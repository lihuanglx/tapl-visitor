package tapl.component.nat

import tapl.common.Exp

trait IsNumVal[A[-X, Y]] {
  def matcher[E]: Matcher[A, E]

  def isNumVal(e: Exp[A]): Option[Int] = matcher[Option[Int]].
    CaseZero(Some(0)).
    CasePred(isNumVal(_).map(_ - 1)).
    CaseSucc(isNumVal(_).map(_ + 1)).
    CaseDefault(None).
    apply(e)
}
