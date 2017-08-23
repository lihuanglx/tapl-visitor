package tapl.component.unit

import tapl.common._
import tapl.component.unit.Term.Factory._
import tapl.component.unit.Term.Query

trait Eval[A[-R, E] <: Term[R, E]] extends Term[Exp[A], Exp[A]] {
  override def tmUnit(): Exp[A] = TmUnit[A]()
}

trait IsVal[A[-R, E]] extends Query[Exp[A], Boolean] {
  override def tmUnit(): Boolean = true
}
