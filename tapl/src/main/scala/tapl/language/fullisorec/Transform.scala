package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with fullsimple.Alg.Transform[A, V] {

  override def TmFold(e: TExp[A, V], t: V): TExp[A, V] = CFold[A, V](apply(e), t)

  override def TmUnfold(e: TExp[A, V], t: V): TExp[A, V] = CUnFold[A, V](apply(e), t)
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with fullsimple.TAlg.Transform[A] with rectype.TTransform[A]
