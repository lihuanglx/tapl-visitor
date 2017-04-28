package tapl.component.bool

import tapl.common.{DefaultMatcher, Exp}

trait Matcher[A[-X, Y], E] extends Alg[Exp[A], E] with DefaultMatcher[E] {
  private var fTmTrue: Option[E] = None
  private var fTmFalse: Option[E] = None
  private var fTmIf: Option[(Exp[A], Exp[A], Exp[A]) => E] = None

  def CaseTrue(f: E): this.type = {
    fTmTrue = Some(f)
    this
  }

  def CaseFalse(f: E): this.type = {
    fTmFalse = Some(f)
    this
  }

  def CaseIf(f: (Exp[A], Exp[A], Exp[A]) => E): this.type = {
    fTmIf = Some(f)
    this
  }

  override def TmTrue(): E = fTmTrue.getOrElse(fDefault.get.apply())

  override def TmFalse(): E = fTmFalse.getOrElse(fDefault.get.apply())

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): E = fTmIf match {
    case Some(f) => f (e1, e2, e3)
    case None => fDefault.get.apply()
  }
}

trait MatcherImpl[E] extends Matcher[Alg, E] {
  override def apply(e: Exp[Alg]): E = e(this)
}
