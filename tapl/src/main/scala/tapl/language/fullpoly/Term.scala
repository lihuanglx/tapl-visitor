package tapl.language.fullpoly

import gems.Language
import tapl.common._
import tapl.component._

@Language
trait Term[-R, E, -F] extends typed.Term[R, E, F] with extension.Term[R, E, F] with pack.Term[R, E, F] {
  def tmTAbs(x: String, e: R): E

  def tmTApp(e: R, t: F): E
}

@Language
trait Type[-F, T] extends typed.Type[F, T] with extension.Type[F, T] {
  def tyAll(x: String, t: F): T

  def tySome(x: String, t: F): T
}

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
