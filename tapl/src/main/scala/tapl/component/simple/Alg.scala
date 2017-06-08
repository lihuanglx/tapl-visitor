package tapl.component.simple

import macros.Visitor
import tapl.common._
import tapl.component._
import tapl.language.tyarith

@Visitor
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with tyarith.Alg[R, E]
  with floatstring.Alg[R, E] with let.Alg[R, E] with typedrecord.Alg[R, E] {

  def tmUnit(): E

  def tmAscribe(e: R, t: F): E

  def tmFix(e: R): E

  def tmInert(t: F): E
}

@Visitor
trait TAlg[-F, T] extends typed.TAlg[F, T] with tyarith.TAlg[F, T] with typedrecord.TAlg[F, T] {
  def tyUnit(): T

  def tyString(): T

  def tyFloat(): T
}
