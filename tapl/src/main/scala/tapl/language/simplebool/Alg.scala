package tapl.language.simplebool

import macros.Language
import tapl.common._
import tapl.component.{typed, typedbool}

@Language
trait Alg[-R, E, -F] extends typed.Alg[R, E, F] with typedbool.Alg[R, E]

@Language
trait TAlg[-F, T] extends typed.TAlg[F, T] with typedbool.TAlg[F, T]

trait Impl[T] extends Alg[Exp2[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: Exp2[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
