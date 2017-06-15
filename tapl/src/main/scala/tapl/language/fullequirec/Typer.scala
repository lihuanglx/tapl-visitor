package tapl.language.fullequirec

import tapl.common._
import tapl.language.fullequirec.TAlg.Factory._
import tapl.component.{extension, variant}
import tapl.language.equirec

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]]
  with equirec.Typer[A, B] with extension.Typer[A, B] with variant.Typer[A, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean = _ (TEquals)(Set.empty)

  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with equirec.TEquals[A] {

  type T = Set[(Exp[A], Exp[A])] => Exp[A] => Boolean

  private def defaultEq(t: Exp[A], f: T): T =
    c => u => c((t, u)) || (u match {
      case TyRec(_, _) => apply(u)(c)(t)
      case _ => f(c)(u)
    })

  override def tyUnit(): T = defaultEq(TyUnit[A](), _ => {
    case TyUnit() => true
    case _ => false
  })

  override def tyFloat(): T = defaultEq(TyFloat[A](), _ => {
    case TyFloat() => true
    case _ => false
  })

  override def tyNat(): T = defaultEq(TyNat[A](), _ => {
    case TyNat() => true
    case _ => false
  })

  override def tyBool(): T = defaultEq(TyBool[A](), _ => {
    case TyBool() => true
    case _ => false
  })

  // todo
  override def tyRecord(l: List[(String, Exp[A])]): T = defaultEq(TyRecord(l), c => {
    case TyRecord(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(c)(t2) && b
        case _ => false
      }
    })
    case _ => false
  })

  override def tyVariant(l: List[(String, Exp[A])]): T = defaultEq(TyVariant(l), c => {
    case TyVariant(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(c)(t2) && b
        case _ => false
      }
    })
    case _ => false
  })

  override def tyString(): T = defaultEq(TyString[A](), _ => {
    case TyString() => true
    case _ => false
  })
}

object TEquals extends TEquals[TAlg] with TImpl[Set[(Exp[TAlg], Exp[TAlg])] => Exp[TAlg] => Boolean] {
  override def subst(m: Map[String, Exp[TAlg]]): TAlg[Exp[TAlg], Exp[TAlg]] = new TSubstImpl(m)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TAlg.Transform[A] with equirec.TSubst[A]

class TSubstImpl(mp: Map[String, Exp[TAlg]]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val m: Map[String, Exp[TAlg]] = mp
}
