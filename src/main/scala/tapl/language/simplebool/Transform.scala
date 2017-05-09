package tapl.language.simplebool

import tapl.common.Util.E3
import tapl.component.{typed, typedbool}

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with typed.Transform[A, V] with typedbool.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
