package tapl.language.fullsimple

import tapl.common._
import tapl.component.{simple, variant}

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with simple.Transform[A, V] with variant.Transform[A, V]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with simple.TTransform[A] with variant.TTransform[A]
