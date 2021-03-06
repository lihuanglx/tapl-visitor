package tapl.language.fullpoly

import tapl.common._
import tapl.component._
import tapl.language.fullpoly.Term.Factory._
import tapl.language.fullpoly.Type.Factory._

trait Parse[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends typed.Parse[A, B] with extension.Parse[A, B] with pack.Parse[A, B] {

  lexical.reserved += ("All", "Some")
  lexical.delimiters += (".", ",", "{", "}", "[", "]")

  lazy val pPolyE: Parser[Exp2[A, Exp[B]]] =
    "\\" ~> ucid ~ ("." ~> pE) ^^ { case x ~ ex => TmTAbs(x, ex) } |||
      pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => TmTApp(ex, ty) }

  lazy val pPolyT: Parser[Exp[B]] =
    "All" ~> ucid ~ ("." ~> pT) ^^ { case x ~ ty => TyAll(x, ty) } |||
      ("{" ~> "Some" ~> ucid ~ ("," ~> pT) <~ "}") ^^ { case x ~ ty => TySome(x, ty) }

  lazy val pFullPolyE: Parser[Exp2[A, Exp[B]]] = pTypedE ||| pExtensionE ||| pPolyE ||| pPackE
  lazy val pFullPolyT: Parser[Exp[B]] = pTypedT ||| pExtensionT ||| pPolyT

  override lazy val pE: Parser[Exp2[A, Exp[B]]] = pFullPolyE
  override lazy val pT: Parser[Exp[B]] = pFullPolyT
}
