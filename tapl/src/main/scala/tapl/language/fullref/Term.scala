package tapl.language.fullref

import macros.Language
import tapl.common._
import tapl.component.{variant, ref}
import tapl.language.fullsub

@Language
trait Term[-R, E, -F] extends fullsub.Term[R, E, F] with variant.Term[R, E, F] with ref.Term[R, E]

@Language
trait Type[-F, T] extends fullsub.Type[F, T] with variant.Type[F, T] with ref.Type[F, T] {
  def tySource(t: F): T

  def tySink(t: F): T
}

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
