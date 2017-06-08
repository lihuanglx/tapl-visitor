package tapl.language.equirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V] with typed.Alg.Transform[A, V]

trait TTransform[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A]]
  with rectype.TTransform[A] with typevar.TTransform[A] with typed.TAlg.Transform[A]
