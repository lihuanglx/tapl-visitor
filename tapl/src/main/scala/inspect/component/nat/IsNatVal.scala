package inspect.component.nat

import inspect.common._
import inspect.component.nat.Term._

trait IsNatVal[A[-X, Y] <: Term[X, Y]] extends Inspect[A] {
  // Int value and predecessor
  def isNatVal(e: Exp[A]): Option[(Int, Exp[A])] = {
    inspectNat(e).flatMap(x => x(new Query[Exp[A], Option[(Int, Exp[A])]] {
      override def default: Option[(Int, Exp[A])] = None

      override def tmZero(): Option[(Int, Exp[A])] = Some((0, e))

      override def tmSucc(e: Exp[A]): Option[(Int, Exp[A])] = for {
        (i, _) <- isNatVal(e)
      } yield (i + 1, e)

      override def apply(e: Exp[A]): Option[(Int, Exp[A])] = impossible
    }))
  }
}
