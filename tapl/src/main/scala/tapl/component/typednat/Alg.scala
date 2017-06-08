package tapl.component.typednat

import macros.Visitor
import tapl.common._
import tapl.component.nat

@Visitor
trait Alg[-R, E] extends nat.Alg[R, E]

@Visitor
trait TAlg[-F, T] {
  def tyNat(): T

  def apply(t: F): T
}
