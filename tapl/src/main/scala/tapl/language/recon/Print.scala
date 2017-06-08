package tapl.language.recon

import tapl.common._
import tapl.component.typed
import tapl.language.tyarith

trait Print[A[-R, E, -F], V] extends Alg[TExp[A, V], String, V]
  with tyarith.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with typed.Print[A, V]
