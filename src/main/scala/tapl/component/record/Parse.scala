package tapl.component.record

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

  private lazy val pRec =
    "{" ~> repsep(lcid ~ ("=" ~> pE) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ CRecord[A]

  private lazy val pProj = pE ~ ("." ~> lcid) ^^ { case e ~ x => CProj(e, x) }

  lazy val pRecordE: Parser[Exp[A]] = pRec ||| pProj
}
