package tapl.component.extension

import tapl.common._
import macros.Language
import tapl.component._
import tapl.language.tyarith

@Language
trait Term[-R, E, -F] extends tyarith.Term[R, E] with floatstring.Term[R, E]
  with let.Term[R, E] with typedrecord.Term[R, E] with unit.Term[R, E] {

  def tmAscribe(e: R, t: F): E

  def tmFix(e: R): E
}

@Language
trait Type[-F, T] extends tyarith.Type[F, T] with typedrecord.Type[F, T] with unit.Type[F, T] {
  def tyString(): T

  def tyFloat(): T
}
