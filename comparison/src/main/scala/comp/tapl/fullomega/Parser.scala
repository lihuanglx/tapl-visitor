package comp.tapl.fullomega

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.{ImplicitConversions, PackratParsers}

object Parser extends StandardTokenParsers with PackratParsers with ImplicitConversions {
  lexical.reserved += ("Bool", "true", "false", "if", "then", "else",
    "Nat", "String", "Unit", "Float", "unit", "case", "let", "in", "succ", "pred",
    "as", "of", "fix", "iszero", "letrec", "Star", "All", "Some", "Ref", "ref")
  lexical.delimiters += ("\\", "(", ")", ";", "/", ".", ":", "->", "=", "!", ":=",
    "<", ">", "{", "}", "=>", ",", "|", "*", "[", "]")

  // lower-case identifier
  lazy val lcid: PackratParser[String] = ident ^? { case id if id.charAt(0).isLower => id }
  // upper-case identifier
  lazy val ucid: PackratParser[String] = ident ^? { case id if id.charAt(0).isUpper => id }

  type Res[A] = Context => A

  // KINDS
  lazy val kind: PackratParser[Res[Kind]] = arrowKind
  lazy val arrowKind: PackratParser[Res[Kind]] =
    (aKind <~ "=>") ~ arrowKind ^^ { case k1 ~ k2 => ctx: Context => KnArr(k1(ctx), k2(ctx)) } |
      aKind
  lazy val aKind: PackratParser[Res[Kind]] =
    "Star" ^^ { _ => ctx: Context => KnStar } |
      "(" ~> kind <~ ")"

  // optional Kind
  lazy val oKind: PackratParser[Res[Kind]] =
    ":" ~> kind |
      success({ ctx: Context => KnStar })

  // TYPES
  lazy val `type`: PackratParser[Res[Ty]] =
    arrowType |
      ("All" ~> ucid) ~ oKind ~ ("." ~> `type`) ^^ { case id ~ k ~ ty => ctx: Context => TyAll(id, k(ctx), ty(ctx.addName(id))) } |
      "Ref" ~> aType ^^ { ty => ctx: Context => TyRef(ty(ctx)) } |
      ("\\" ~> ucid) ~ oKind ~ ("." ~> `type`) ^^ { case id ~ k ~ ty => ctx: Context => TyAbs(id, k(ctx), ty(ctx.addName(id))) }

  lazy val aType: PackratParser[Res[Ty]] =
    "(" ~> `type` <~ ")" |
      ucid ^^ { tn => ctx: Context => if (ctx.isNameBound(tn)) TyVar(ctx.name2index(tn), ctx.length) else TyId(tn) } |
      "Bool" ^^ { _ => ctx: Context => TyBool } |
      "String" ^^ { _ => ctx: Context => TyString } |
      "Unit" ^^ { _ => ctx: Context => TyUnit } |
      "{" ~> fieldTypes <~ "}" ^^ { ft => ctx: Context => TyRecord(ft(ctx)) } |
      "Nat" ^^ { _ => ctx: Context => TyNat } |
      (("{" ~ "Some") ~> ucid) ~ oKind ~ ("," ~> `type` <~ "}") ^^ { case id ~ k ~ ty => ctx: Context => TySome(id, k(ctx), ty(ctx.addName(id))) }

  lazy val fieldTypes: PackratParser[Res[List[(String, Ty)]]] =
    repsep(fieldType, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }

  lazy val fieldType: PackratParser[(Context, Int) => (String, Ty)] =
    lcid ~ (":" ~> `type`) ^^ { case id ~ ty => (ctx: Context, i: Int) => (id, ty(ctx)) } |
      `type` ^^ { ty => (ctx: Context, i: Int) => (i.toString, ty(ctx)) }

  lazy val arrowType: PackratParser[Res[Ty]] =
    (appType <~ "->") ~ arrowType ^^ { case t1 ~ t2 => ctx: Context => TyArr(t1(ctx), t2(ctx)) } |
      appType

  lazy val appType: PackratParser[Res[Ty]] =
    appType ~ aType ^^ { case t1 ~ t2 => ctx: Context => TyApp(t1(ctx), t2(ctx)) } |
      aType

  // TERMS
  lazy val term: PackratParser[Res[Term]] =
    ("\\" ~> lcid) ~ (":" ~> `type`) ~ ("." ~> term) ^^ { case v ~ ty ~ t => ctx: Context => TmAbs(v, ty(ctx), t(ctx.addName(v))) } |
      (appTerm <~ ":=") ~ appTerm ^^ { case t1 ~ t2 => ctx: Context => TmAssign(t1(ctx), t2(ctx)) } |
      ("let" ~> lcid) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ t1 ~ t2 => ctx: Context => TmLet(id, t1(ctx), t2(ctx.addName(id))) } |
      ("if" ~> term) ~ ("then" ~> term) ~ ("else" ~> term) ^^ { case t1 ~ t2 ~ t3 => ctx: Context => TmIf(t1(ctx), t2(ctx), t3(ctx)) } |
      ("letrec" ~> lcid) ~ (":" ~> `type`) ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id ~ ty ~ t1 ~ t2 => ctx: Context => TmLet(id, TmFix(TmAbs(id, ty(ctx), t1(ctx.addName(id)))), t2(ctx.addName(id))) } |
      (("let" ~ "{") ~> ucid) ~ ("," ~> lcid <~ "}") ~ ("=" ~> term) ~ ("in" ~> term) ^^ { case id1 ~ id2 ~ t1 ~ t2 => ctx: Context => TmUnPack(id1, id2, t1(ctx), t2(ctx.addName(id1).addName(id2))) } |
      ("\\" ~> ucid) ~ oKind ~ ("." ~> term) ^^ { case id ~ k ~ t => ctx: Context => TmTAbs(id, k(ctx), t(ctx.addName(id))) } |
      appTerm

  lazy val appTerm: PackratParser[Res[Term]] =
    (appTerm <~ "[") ~ (`type` <~ "]") ^^ { case t ~ ty => ctx: Context => TmTApp(t(ctx), ty(ctx)) } |
      appTerm ~ pathTerm ^^ { case t1 ~ t2 => ctx: Context => TmApp(t1(ctx), t2(ctx)) } |
      "ref" ~> pathTerm ^^ { t => ctx: Context => TmRef(t(ctx)) } |
      "!" ~> pathTerm ^^ { t => ctx: Context => TmDeref(t(ctx)) } |
      "fix" ~> pathTerm ^^ { t => ctx: Context => TmFix(t(ctx)) } |
      "succ" ~> pathTerm ^^ { t => ctx: Context => TmSucc(t(ctx)) } |
      "pred" ~> pathTerm ^^ { t => ctx: Context => TmPred(t(ctx)) } |
      "iszero" ~> pathTerm ^^ { t => ctx: Context => TmIsZero(t(ctx)) } |
      pathTerm

  lazy val ascribeTerm: PackratParser[Res[Term]] =
    aTerm ~ ("as" ~> `type`) ^^ { case t ~ ty => ctx: Context => TmAscribe(t(ctx), ty(ctx)) } |
      aTerm

  lazy val pathTerm: PackratParser[Res[Term]] =
    pathTerm ~ ("." ~> lcid) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      pathTerm ~ ("." ~> numericLit) ^^ { case t1 ~ l => ctx: Context => TmProj(t1(ctx), l) } |
      ascribeTerm

  lazy val termSeq: PackratParser[Res[Term]] =
    term ~ (";" ~> termSeq) ^^ { case t ~ ts => ctx: Context => TmApp(TmAbs("_", TyUnit, ts(ctx.addName("_"))), t(ctx)) } |
      term

  lazy val aTerm: PackratParser[Res[Term]] =
    "(" ~> term <~ ")" |
      "{" ~> termSeq <~ "}" |
      ("inert" ~ "[") ~> `type` <~ "]" ^^ { ty => ctx: Context => TmInert(ty(ctx)) } |
      "true" ^^ { _ => ctx: Context => TmTrue } |
      "false" ^^ { _ => ctx: Context => TmFalse } |
      lcid ^^ { i => ctx: Context => TmVar(ctx.name2index(i), ctx.length) } |
      stringLit ^^ { l => ctx: Context => TmString(l) } |
      "unit" ^^ { _ => ctx: Context => TmUnit } |
      "{" ~> fields <~ "}" ^^ { fs => ctx: Context => TmRecord(fs(ctx)) } |
      numericLit ^^ { x => ctx: Context => num(x.toInt) } |
      (("{" ~ "*") ~> `type`) ~ ("," ~> term <~ "}") ~ ("as" ~> `type`) ^^ { case ty1 ~ t ~ ty2 => ctx: Context => TmPack(ty1(ctx), t(ctx), ty2(ctx)) }

  lazy val fields: PackratParser[Res[List[(String, Term)]]] =
    repsep(field, ",") ^^ { fs => ctx: Context => fs.zipWithIndex.map { case (ft, i) => ft(ctx, i + 1) } }
  lazy val field: PackratParser[(Context, Int) => (String, Term)] =
    lcid ~ ("=" ~> term) ^^ { case id ~ t => (ctx: Context, i: Int) => (id, t(ctx)) } |
      term ^^ { t => (ctx: Context, i: Int) => (i.toString, t(ctx)) }

  private def num(x: Int): Term = x match {
    case 0 => TmZero
    case _ => TmSucc(num(x - 1))
  }

  def input(s: String) = phrase(term)(new lexical.Scanner(s)) match {
    case t if t.successful => t.get
    case t => sys.error(t.toString)
  }

}
