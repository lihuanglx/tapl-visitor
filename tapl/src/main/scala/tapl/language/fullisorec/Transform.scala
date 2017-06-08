package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with fullsimple.Transform[A, V] {

  override def TmFold(e: E3[A, V], t: V): E3[A, V] = CFold[A, V](apply(e), t)

  override def TmUnfold(e: E3[A, V], t: V): E3[A, V] = CUnFold[A, V](apply(e), t)
}

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with fullsimple.TTransform[A] with rectype.TTransform[A]
