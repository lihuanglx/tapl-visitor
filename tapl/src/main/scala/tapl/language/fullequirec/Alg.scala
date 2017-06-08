package tapl.language.fullequirec

import tapl.common._
import tapl.component.{rectype, typevar}
import tapl.language.fullsimple

trait Alg[-R, E, -F] extends fullsimple.Alg[R, E, F]

trait Factory extends fullsimple.Factory

object Factory extends Factory

trait TAlg[-F, T] extends fullsimple.TAlg[F, T] with typevar.TAlg[F, T] with rectype.TAlg[F, T]

trait TFactory extends fullsimple.TFactory with typevar.TFactory with rectype.TFactory

object TFactory extends TFactory

trait Impl[T] extends Alg[E3[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: E3[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}