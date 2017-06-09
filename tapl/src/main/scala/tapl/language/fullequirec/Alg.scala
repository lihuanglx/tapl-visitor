package tapl.language.fullequirec

import macros.Visitor
import tapl.common._
import tapl.component.{rectype, typevar}
import tapl.language.fullsimple

@Visitor
trait Alg[-R, E, -F] extends fullsimple.Alg[R, E, F]

@Visitor
trait TAlg[-F, T] extends fullsimple.TAlg[F, T] with typevar.TAlg[F, T] with rectype.TAlg[F, T]

trait Impl[T] extends Alg[TExp[Alg, Exp[TAlg]], T, Exp[TAlg]] {
  override def apply(e: TExp[Alg, Exp[TAlg]]): T = e(this)
}

trait TImpl[T] extends TAlg[Exp[TAlg], T] {
  override def apply(t: Exp[TAlg]): T = t(this)
}
