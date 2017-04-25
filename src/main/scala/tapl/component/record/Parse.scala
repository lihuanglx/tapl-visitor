package tapl.component.record

import tapl.common.{EParser, Exp}

trait Parse[A[-X, Y] <: Alg[X, Y]] extends EParser[A] {
  lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

  val f: Factory[A]

  private lazy val pRec =
    "{" ~> repsep(lcid ~ ("=" ~> pE) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ f.TmRecord

  private lazy val pProj = pE ~ ("." ~> lcid) ^^ { case e ~ x => f.TmProj(e, x) }

  lazy val pRecordE: Parser[Exp[A]] = pRec ||| pProj
}
