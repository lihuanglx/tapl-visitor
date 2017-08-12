package tapl.language.fullpoly

import macros.Language
import tapl.common._
import tapl.component._

@Language
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with extension.Alg[R, E, F] with pack.Alg[R, E, F] {
  def tmTAbs(x: String, e: R): E

  def tmTApp(e: R, t: F): E
}

@Language
trait TAlg[-F, T] extends typed.TAlg[F, T] with extension.TAlg[F, T] {
  def tyAll(x: String, t: F): T

  def tySome(x: String, t: F): T
}

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
