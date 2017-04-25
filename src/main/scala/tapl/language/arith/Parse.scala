package tapl.language.arith

import tapl.common.Exp
import tapl.component._

trait Parse[A[-X, Y] <: Alg[X, Y]] extends bool.Parse[A] with nat.Parse[A] {
  override val f: Factory[A]

  lazy val pArithE: Parser[Exp[A]] = pBoolE ||| pNatE

  override lazy val pE: Parser[Exp[A]] = pArithE

  override def parse(inp: String): Option[Exp[A]] = parseBy(pE)(inp)
}
