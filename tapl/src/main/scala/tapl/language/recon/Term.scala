package tapl.language.recon

import gems.Language
import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

@Language
trait Term[-R, E, -F] extends tyarith.Term[R, E] with typed.Term[R, E, F]

@Language
trait Type[-F, T] extends tyarith.Type[F, T] with typed.Type[F, T]

trait Impl[T] extends Term[Exp2[Term, Exp[Type]], T, Exp[Type]] {
  override def apply(e: Exp2[Term, Exp[Type]]): T = e(this)
}

trait TImpl[T] extends Type[Exp[Type], T] {
  override def apply(t: Exp[Type]): T = t(this)
}
