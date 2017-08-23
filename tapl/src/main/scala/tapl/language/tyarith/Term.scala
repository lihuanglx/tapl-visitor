package tapl.language.tyarith

import macros.Language
import tapl.common._
import tapl.component.{typedbool, typednat}

@Language
trait Term[-R, E] extends typedbool.Term[R, E] with typednat.Term[R, E]

@Language
trait Type[-F, T] extends typedbool.Type[F, T] with typednat.Type[F, T]

trait Impl[T] extends Term[Exp[Term], T] {
  override def apply(e: Exp[Term]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
