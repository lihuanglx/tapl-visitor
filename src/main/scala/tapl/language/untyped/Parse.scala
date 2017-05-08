package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends lambda.Parse[A] with varapp.Parse[A] {
  lazy val pUntypedE: Parser[Exp[A]] = pLamE ||| pVarAppE

  override lazy val pE: Parser[Exp[A]] = pUntypedE
}
