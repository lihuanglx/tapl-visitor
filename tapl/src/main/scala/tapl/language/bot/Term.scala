package tapl.language.bot

import macros.Language
import tapl.common._
import tapl.component.{top, bottom, typed}

@Language
trait Term[-R, E, -F] extends typed.Term[R, E, F]

@Language
trait Type[-F, T] extends typed.Type[F, T] with top.Type[F, T] with bottom.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
