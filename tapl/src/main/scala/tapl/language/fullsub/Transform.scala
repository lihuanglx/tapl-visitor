package tapl.language.fullsub

import tapl.common._
import tapl.component.simple

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[TExp[A, V], TExp[A, V], V]
  with simple.Alg.Transform[A, V]
