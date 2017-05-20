package tapl.language.fulluntyped

import tapl.common._
import tapl.component.{floatstring, let, record}
import tapl.language.{arith, untyped}

trait Transform[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with arith.Transform[A] with untyped.Transform[A]
  with floatstring.Transform[A] with let.Transform[A] with record.Transform[A]
