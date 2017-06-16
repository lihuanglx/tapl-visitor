package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple
import tapl.language.fullisorec.Alg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends fullsimple.Parse[A, B] with rectype.Parse[B] {

  lexical.reserved += ("fold", "unfold")
  lexical.delimiters += ("[", "]")

  private lazy val pFoldE: Parser[Exp2[A, Exp[B]]] =
    "fold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => TmFold[A, Exp[B]](ex, ty) } |||
      "unfold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => TmUnfold[A, Exp[B]](ex, ty) }

  lazy val pFullIsoRecE: Parser[Exp2[A, Exp[B]]] = pFullSimpleE ||| pFoldE
  lazy val pFullIsoRecT: Parser[Exp[B]] = pFullSimpleT ||| pRecTypeT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullIsoRecE
  override lazy val pT: Parser[Exp[B]] = pFullIsoRecT
}
