package tapl.language.rcdsubbot

import gems.Language
import tapl.common._
import tapl.component.typedrecord
import tapl.language.bot

@Language
trait Term[-R, E, -F] extends typedrecord.Term[R, E] with bot.Term[R, E, F]

@Language
trait Type[-F, T] extends typedrecord.Type[F, T] with bot.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
