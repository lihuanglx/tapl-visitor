package tapl.language.fullomega

import gems.Language
import tapl.common._
import tapl.component._

@Language
trait Term[-R, E, -T, -K] extends typed.Term[R, E, T] with extension.Term[R, E, T]
  with pack.Term[R, E, T] with ref.Term[R, E] {

  def tmTAbs(x: String, k: K, e: R): E

  def tmTApp(e: R, t: T): E
}

@Language
trait Type[-F, T, -K] extends typed.Type[F, T] with extension.Type[F, T] with ref.Type[F, T] {
  def tyAll(x: String, k: K, t: F): T

  def tySome(x: String, k: K, t: F): T

  def tyAbs(x: String, k: K, t: F): T

  def tyApp(t1: F, t2: F): T
}

@Language
trait Kind[-K, Y] {
  def knStar(): Y

  def knArr(k1: K, k2: K): Y

  def apply(k: K): Y
}

trait Impl[T] extends Term[Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]], T, Exp2[Type, Exp[Kind]], Exp[Kind]] {
  override def apply(e: Exp3[Term, Exp2[Type, Exp[Kind]], Exp[Kind]]): T = e(this)
}

trait TImpl[T] extends Type[Exp2[Type, Exp[Kind]], T, Exp[Kind]] {
  override def apply(t: Exp2[Type, Exp[Kind]]): T = t(this)
}

trait KImpl[T] extends Kind[Exp[Kind], T] {
  override def apply(k: Exp[Kind]): T = k(this)
}
