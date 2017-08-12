package tapl.component.typedbool

import macros.Language
import tapl.common._
import tapl.component.bool

@Language
trait Alg[-R, E] extends bool.Alg[R, E]

@Language
trait TAlg[-F, T] {
  def tyBool(): T

  def apply(t: F): T
}
