package tapl.language.fullerror

import macros.Language
import tapl.common._
import tapl.component.typedbool
import tapl.language.bot

@Language
trait Term[-R, E, -F] extends bot.Term[R, E, F] with typedbool.Term[R, E] {
  def tmError(): E

  def tmTry(e1: R, e2: R): E
}

@Language
trait Type[-F, T] extends bot.Type[F, T] with typedbool.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
