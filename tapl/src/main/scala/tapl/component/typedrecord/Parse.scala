package tapl.component.typedrecord

import tapl.common._
import tapl.component.record
import tapl.component.typedrecord.TAlg.Factory._

trait Parse[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends TParser[B] with record.Parse[A] {
  lexical.delimiters += (":", ",", "{", "}")

  val pTypedRecordE: Parser[Exp[A]] = pRecordE
  val pTypedRecordT: Parser[Exp[B]] =
    "{" ~> repsep(lcid ~ (":" ~> pT) ^^ { case x ~ e => (x, e) }, ",") <~ "}" ^^ TyRecord[B]
}
