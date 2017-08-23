package tapl.component.record

import tapl.common._
import tapl.component.record.Term.Factory._

trait Parse[A[-X, Y] <: Term[X, Y]] extends EParser[A] {
  lexical.delimiters += ("{", "}", ",", ".", "(", ")", "=")

  private lazy val pRec =
    "{" ~> repsep(lcid ~ ("=" ~> pE) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TmRecord[A]

  private lazy val pProj = pE ~ ("." ~> lcid) ^^ { case e ~ x => TmProj(e, x) }

  lazy val pRecordE: Parser[Exp[A]] = pRec ||| pProj
}
