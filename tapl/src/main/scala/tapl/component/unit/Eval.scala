package tapl.component.unit

import tapl.common._
import tapl.component.unit.Alg.Factory._
import tapl.component.unit.Alg.Query

trait Eval[A[-R, E] <: Alg[R, E]] extends Alg[Exp[A], Exp[A]] {
  override def tmUnit(): Exp[A] = TmUnit[A]()
}

trait IsVal[A[-R, E]] extends Query[Exp[A], Boolean] {
  override def tmUnit(): Boolean = true
}
