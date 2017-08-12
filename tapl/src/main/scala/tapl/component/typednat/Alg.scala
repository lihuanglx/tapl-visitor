package tapl.component.typednat

import macros.Language
import tapl.common._
import tapl.component.nat

@Language
trait Alg[-R, E] extends nat.Alg[R, E]

@Language
trait TAlg[-F, T] {
  def tyNat(): T

  def apply(t: F): T
}
