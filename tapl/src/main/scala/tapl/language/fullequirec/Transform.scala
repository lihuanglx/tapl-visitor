package tapl.language.fullequirec

import tapl.common._
import tapl.component.{rectype, typevar}
import tapl.language.fullsimple

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with fullsimple.Transform[A, V]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with fullsimple.TTransform[A] with typevar.TTransform[A] with rectype.TTransform[A]
