package tapl.language.fullomega

import macros.Visitor
import tapl.common._
import tapl.component._

@Visitor
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with extension.Alg[R, E, F]
  with pack.Alg[R, E, F] with ref.Alg[R, E] {
  // todo
  //def TmTAbs(x: String, k: K, e: E): E

  def tmTApp(e: R, t: F): E
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
