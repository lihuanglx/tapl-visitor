package tapl.language.fullomega

import tapl.common._
import tapl.component._
import tapl.language.fullomega.Alg.Factory._
import tapl.language.fullomega.TAlg.Factory._
import tapl.language.fullomega.KAlg.Factory._

trait Parse[A[-R, E, -T, -K] <: Alg[R, E, T, K], B[-F, T, -K] <: TAlg[F, T, K], C[-X, Y] <: KAlg[X, Y]]
  extends typed.Parse[A[-?, ?, -?, Exp[C]], B[-?, ?, Exp[C]]] with KParser[C]
    with extension.Parse[A[-?, ?, -?, Exp[C]], B[-?, ?, Exp[C]]]
    with pack.Parse[A[-?, ?, -?, Exp[C]], B[-?, ?, Exp[C]]]
    with ref.Parse[A[-?, ?, Exp2[B, Exp[C]], Exp[C]], B[-?, ?, Exp[C]]] {

  lexical.reserved += ("Star", "All", "Some")
  lexical.delimiters += ("=>", ":", ".", ",", "{", "}")

  private lazy val pOmegaE: Parser[Exp3[A, Exp2[B, Exp[C]], Exp[C]]] =
    "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pE) ^^ { case x ~ kn ~ ex => TmTAbs(x, kn, ex) } |||
      pE ~ ("[" ~> pT <~ "]") ^^ { case ex ~ ty => TmTApp(ex, ty) }

  private lazy val pOmegaT: Parser[Exp2[B, Exp[C]]] =
    "All" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => TyAll(x, kn, ty) } |||
      ("{" ~> "Some" ~> ucid ~ (":" ~> pK) ~ ("," ~> pT) <~ "}") ^^ { case x ~ kn ~ ty => TySome(x, kn, ty) } |||
      "\\" ~> ucid ~ (":" ~> pK) ~ ("." ~> pT) ^^ { case x ~ kn ~ ty => TyAbs(x, kn, ty) } |||
      pT ~ pT ^^ { case t1 ~ t2 => TyApp(t1, t2) }

  private lazy val pOmegaK: Parser[Exp[C]] =
    "Star" ^^^ KnStar[C]() |||
      pK ~ ("=>" ~> pK) ^^ { case k1 ~ k2 => KnArr[C](k1, k2) } |||
      "(" ~> pK <~ ")"

  lazy val pFullOmegaE: Parser[Exp3[A, Exp2[B, Exp[C]], Exp[C]]] =
    pTypedE ||| pExtensionE ||| pPackE ||| pRefE ||| pOmegaE
  lazy val pFullOmegaT: Parser[Exp2[B, Exp[C]]] =
    pTypedT ||| pExtensionT ||| pRefT ||| pOmegaT
  lazy val pFullOmegaK: Parser[Exp[C]] = pOmegaK

  override lazy val pE: Parser[Exp3[A, Exp2[B, Exp[C]], Exp[C]]] = pFullOmegaE
  override lazy val pT: Parser[Exp2[B, Exp[C]]] = pFullOmegaT
  override lazy val pK: Parser[Exp[C]] = pFullOmegaK

}
