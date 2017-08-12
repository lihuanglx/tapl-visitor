package tapl.language.tyarith

import macros.Language
import tapl.common._
import tapl.component.{typedbool, typednat}

@Language
trait Alg[-R, E] extends typedbool.Alg[R, E] with typednat.Alg[R, E]

@Language
trait TAlg[-F, T] extends typedbool.TAlg[F, T] with typednat.TAlg[F, T]

trait Impl[T] extends Alg[Exp[Alg], T] {
  override def apply(e: Exp[Alg]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
