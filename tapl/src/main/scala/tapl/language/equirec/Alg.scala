package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Alg[-R, E, -F] extends typed.Alg[R, E, F]

trait Factory extends typed.Factory

object Factory extends Factory

trait TAlg[-F, T] extends typed.TAlg[F, T] with typevar.TAlg[F, T] with rectype.TAlg[F, T]

trait TFactory extends typed.TFactory with typevar.TFactory with rectype.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
