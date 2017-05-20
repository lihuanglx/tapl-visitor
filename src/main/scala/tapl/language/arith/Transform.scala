package tapl.language.arith

import tapl.common._
import tapl.component._

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with bool.Transform[A] with nat.Transform[A]