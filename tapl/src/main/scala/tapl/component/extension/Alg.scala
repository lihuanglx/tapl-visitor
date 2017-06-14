package tapl.component.extension

import tapl.common._
import macros.Visitor
import tapl.component._
import tapl.language.tyarith

@Visitor
trait Alg[-R, E, -F] extends tyarith.Alg[R, E] with floatstring.Alg[R, E]
  with let.Alg[R, E] with typedrecord.Alg[R, E] with unit.Alg[R, E] {

  def tmAscribe(e: R, t: F): E

  def tmFix(e: R): E
}

@Visitor
trait TAlg[-F, T] extends tyarith.TAlg[F, T] with typedrecord.TAlg[F, T] with unit.TAlg[F, T] {
  def tyString(): T

  def tyFloat(): T
}
