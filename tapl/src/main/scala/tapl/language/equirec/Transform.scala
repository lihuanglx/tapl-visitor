package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V] with typed.Transform[A, V]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with rectype.TTransform[A] with typevar.TTransform[A] with typed.TTransform[A]
