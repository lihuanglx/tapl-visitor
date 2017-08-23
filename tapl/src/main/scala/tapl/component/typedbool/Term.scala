package tapl.component.typedbool

import macros.Language
import tapl.common._
import tapl.component.bool

@Language
trait Term[-R, E] extends bool.Term[R, E]

@Language
trait Type[-F, T] {
  def tyBool(): T

  def apply(t: F): T
}
