package tapl.language.fullrecon

import tapl.common._
import tapl.component.let
import tapl.language.recon

trait Print[A[-R, E, -F], V] extends Alg[E3[A, V], String, V]
  with let.Print[({type lam[-X, Y] = A[X, Y, V]})#lam] with recon.Print[A, V]
