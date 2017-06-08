package tapl.language.fullisorec

import tapl.common._
import tapl.component.rectype
import tapl.language.fullsimple

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends fullsimple.Parse[A, B] with rectype.Parse[B] {

  lexical.reserved += ("fold", "unfold")
  lexical.delimiters += ("[", "]")

  private lazy val pFoldE: Parser[TExp[A, Exp[B]]] =
    "fold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => CFold[A, Exp[B]](ex, ty) } |||
      "unfold" ~> ("[" ~> pT <~ "]") ~ pE ^^ { case ty ~ ex => CUnFold[A, Exp[B]](ex, ty) }

  lazy val pFullIsoRecE: Parser[TExp[A, Exp[B]]] = pFullSimpleE ||| pFoldE
  lazy val pFullIsoRecT: Parser[Exp[B]] = pFullSimpleT ||| pRecTypeT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullIsoRecE
  override lazy val pT: Parser[Exp[B]] = pFullIsoRecT
}
