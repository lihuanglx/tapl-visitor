package tapl.component.rectype

import tapl.common._
import tapl.component.rectype.TAlg.Factory._
import tapl.component.typevar

trait Parse[A[-X, Y] <: TAlg[X, Y]] extends typevar.Parse[A] {
  lexical.reserved += "Rec"
  lexical.delimiters += "."

  val pRecTypeT: Parser[Exp[A]] =
    "Rec" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => TyRec[A](x, ty) } ||| pTypeVarT
}
