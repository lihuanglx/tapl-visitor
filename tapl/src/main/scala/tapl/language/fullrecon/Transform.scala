package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with recon.Transform[A, V] with let.Alg.Transform[({type lam[-X, Y] = A[X, Y, V]})#lam]
