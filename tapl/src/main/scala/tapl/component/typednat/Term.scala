package tapl.component.typednat

import gems.Language
import tapl.common._
import tapl.component.nat

@Language
trait Term[-R, E] extends nat.Term[R, E]

@Language
trait Type[-F, T] {
  def tyNat(): T

  def apply(t: F): T
}
