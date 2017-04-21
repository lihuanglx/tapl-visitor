package tapl.language.untyped

import tapl.common.Exp
import tapl.component._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends lambda.Parse[A] with varapp.Parse[A] {
  override val f: Factory[A]

  lazy val pUntypedE: Parser[Exp[A]] = pLamE ||| pVarAppE

  override val pE: Parser[Exp[A]] = pUntypedE

  // todo
  override def parse(inp: String): Option[Exp[A]] = parseBy(pE)(inp)
}
