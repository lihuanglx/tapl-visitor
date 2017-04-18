package tapl.language.arith

import tapl.component._

object Syntax {

  trait Alg[-R, E] extends bool.Alg[R, E] with nat.Alg[R, E]

  //type Exp = tapl.common.Exp[Alg]

  trait Factory[A[-X, Y] <: Alg[X, Y]] extends bool.Factory[A] with nat.Factory[A]

  object Factory extends Factory[Alg]

}