package tapl.component.rectype

import tapl.common._

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends TParser[A] {
  lexical.reserved += "Rec"
  lexical.delimiters += "."

  val pRecTypeT: Parser[Exp[A]] = "Rec" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => CTyRec[A](x, ty) }
}
