package tapl.language.fullomega

import macros.Visitor
import tapl.common._
import tapl.component._

@Visitor
trait Alg[-R, E, -T, -K] extends typed.Alg[R, E, T] with extension.Alg[R, E, T]
  with pack.Alg[R, E, T] with ref.Alg[R, E] {

  def tmTAbs(x: String, k: K, e: R): E

  def tmTApp(e: R, t: T): E
}

@Visitor
trait TAlg[-F, T, -K] extends typed.TAlg[F, T] with extension.TAlg[F, T]
  with ref.TAlg[F, T] with typevar.TAlg[F, T] {

  def tyAll(x: String, k: K, t: F): T

  def tySome(x: String, k: K, t: F): T

  def tyAbs(x: String, k: K, t: F): T

  def tyApp(t1: F, t2: F): T
}

@Visitor
trait KAlg[-K, Y] {
  def knStar(): Y

  def knArr(k1: K, k2: K): Y

  def apply(k: K): Y
}
