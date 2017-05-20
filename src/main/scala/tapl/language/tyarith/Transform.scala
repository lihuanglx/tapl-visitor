package tapl.language.tyarith

import tapl.common._
import tapl.component.{typedbool, typednat}

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with typedbool.Transform[A] with typednat.Transform[A]
