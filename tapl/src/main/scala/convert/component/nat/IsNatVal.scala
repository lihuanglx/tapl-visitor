package convert.component.nat

import convert.common._
import convert.component.nat.Term._

trait IsNatVal[A[-X, Y] <: Term[X, Y]] extends Convert[A] {
  // Int value and predecessor
  def isNatVal(e: Exp[A]): Option[(Int, Exp[A])] = {
    convertNat(e).flatMap(x => x(new Query[Exp[A], Option[(Int, Exp[A])]] {
      override def default: Option[(Int, Exp[A])] = None

      override def tmZero(): Option[(Int, Exp[A])] = Some((0, e))

      override def tmSucc(e: Exp[A]): Option[(Int, Exp[A])] = for {
        (i, _) <- isNatVal(e)
      } yield (i + 1, e)

      override def apply(e: Exp[A]): Option[(Int, Exp[A])] = impossible
    }))
  }
}
