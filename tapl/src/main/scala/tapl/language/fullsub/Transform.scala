package tapl.language.fullsub

import tapl.common._
import tapl.component.simple

trait Transform[A[-R, E, -F] <: Alg[R, E, F], V] extends Alg[E3[A, V], E3[A, V], V]
  with simple.Transform[A, V]
