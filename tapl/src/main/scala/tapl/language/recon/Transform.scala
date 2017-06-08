package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with typed.Alg.Transform[A, V] with tyarith.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
