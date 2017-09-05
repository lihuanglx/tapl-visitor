package tapl.language.simplebool

import gems.Language
import tapl.common._
import tapl.component.{typed, typedbool}

@Language
trait Term[-R, E, -F] extends typed.Term[R, E, F] with typedbool.Term[R, E]

@Language
trait Type[-F, T] extends typed.Type[F, T] with typedbool.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
