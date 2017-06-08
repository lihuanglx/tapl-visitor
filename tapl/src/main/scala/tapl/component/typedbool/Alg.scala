package tapl.component.typedbool

import macros.Visitor
import tapl.common._
import tapl.component.bool

@Visitor("debug")
trait Alg[-R, E] extends bool.Alg[R, E]

@Visitor("debug")
trait TAlg[-F, T] {
  def tyBool(): T

  def apply(t: F): T
}
