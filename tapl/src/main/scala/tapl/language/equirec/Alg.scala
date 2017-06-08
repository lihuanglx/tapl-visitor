package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Alg[-R, E, -F] extends typed.Alg[R, E, F]

trait Factory extends typed.Alg.Factory

object Factory extends Factory

trait TAlg[-F, T] extends typed.TAlg[F, T] with typevar.TAlg[F, T] with rectype.TAlg[F, T]

trait TFactory extends typed.TAlg.Factory with typevar.TFactory with rectype.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
