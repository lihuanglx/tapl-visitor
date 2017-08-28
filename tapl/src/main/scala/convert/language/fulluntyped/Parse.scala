package convert.language.fulluntyped

import convert.common._
import convert.component.{floatstring, let, record}
import convert.language.{arith, untyped}

trait Parse[A[-X, Y] <: Term[X, Y]] extends arith.Parse[A] with untyped.Parse[A]
  with floatstring.Parse[A] with let.Parse[A] with record.Parse[A] {

  lazy val pFullUntypedE: Parser[Exp[A]] = pArithE ||| pUntypedE ||| pRecordE ||| pFloatStringE ||| pLetE

  override lazy val pE: Parser[Exp[A]] = pFullUntypedE
}
