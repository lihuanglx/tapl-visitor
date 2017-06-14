package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Alg.Factory._
import tapl.language.fullpoly.TAlg.Factory._

trait Parse[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends typed.Parse[A, B] with extension.Parse[A, B] with pack.Parse[A, B] with typevar.Parse[B] {

  lexical.reserved += ("All", "Some")
  lexical.delimiters += (".", ",", "{", "}", "[", "]")

  lazy val pPolyE: Parser[TExp[A, Exp[B]]] =
    "\\" ~> ucid ~ ("." ~> pE) ^^ { case x ~ ex => TmTAbs(x, ex) } |||
      pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => TmTApp(ex, ty) }

  lazy val pPolyT: Parser[Exp[B]] =
    "All" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => TyAll(x, ty) } |||
      ("{" ~> "Some" ~> ucid ~ ("," ~> pT) <~ "}") ^^ { case x ~ ty => TySome(x, ty) }

  lazy val pFullPolyE: Parser[TExp[A, Exp[B]]] = pTypedE ||| pExtensionE ||| pPolyE ||| pPackE
  lazy val pFullPolyT: Parser[Exp[B]] = pTypedT ||| pExtensionT ||| pPolyT ||| pTypeVarT

  override lazy val pE: Parser[TExp[A, Exp[B]]] = pFullPolyE
  override lazy val pT: Parser[Exp[B]] = pFullPolyT
}
