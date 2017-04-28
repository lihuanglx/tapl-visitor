package tapl.component.nat

import tapl.common.{DefaultMatcher, Exp}

trait Matcher[A[-X, Y], E] extends Alg[Exp[A], E] with DefaultMatcher[E] {
  private var fTmZero: Option[E] = None
  private var fTmSucc: Option[Exp[A] => E] = None
  private var fTmPred: Option[Exp[A] => E] = None
  private var fTmIsZero: Option[Exp[A] => E] = None

  def CaseZero(f: E): this.type = {
    fTmZero = Some(f)
    this
  }

  def CaseSucc(f: Exp[A] => E): this.type = {
    fTmSucc = Some(f)
    this
  }

  def CasePred(f: Exp[A] => E): this.type = {
    fTmPred = Some(f)
    this
  }

  def CaseIsZero(f: Exp[A] => E): this.type = {
    fTmIsZero = Some(f)
    this
  }

  override def TmZero(): E = fTmZero.getOrElse(fDefault.get.apply())

  override def TmPred(e: Exp[A]): E = fTmPred match {
    case Some(f) => f(e)
    case None => fDefault.get.apply()
  }

  override def TmSucc(e: Exp[A]): E = fTmSucc match {
    case Some(f) => f(e)
    case None => fDefault.get.apply()
  }

  override def TmIsZero(e: Exp[A]): E = fTmIsZero match {
    case Some(f) => f(e)
    case None => fDefault.get.apply()
  }
}

trait MatcherImpl[E] extends Matcher[Alg, E] {
  override def apply(e: Exp[Alg]): E = e(this)
}
