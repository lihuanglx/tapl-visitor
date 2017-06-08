package tapl.language.fullequirec

import tapl.common._
import tapl.component.{rectype, typed, typevar}
import tapl.language.fullequirec.TFactory._
import tapl.language.fullsimple

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]] with fullsimple.Typer[A, B] with ISubst[B] {

  override def tmApp(e1: TExp[A, Exp[B]], e2: TExp[A, Exp[B]]): Type[B] = c => {
    def go(t: Exp[B]): Exp[B] = t match {
      case TyRec(x, r) => go(r(subst(x, t)))
      case TyArr(t1, t2) if t1(tEquals)(apply(e2)(c)) => t2
      case _ => typeError()
    }

    go(apply(e1)(c))
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = TEquals

  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals2[A] with rectype.TEquals[A] {

  override def tyVariant(l: List[(String, Exp[A])]): Exp[A] => Boolean = recEq.tyVariant(l)(Set.empty)

  override def tyUnit(): Exp[A] => Boolean = recEq.tyUnit()(Set.empty)

  override def tyFloat(): Exp[A] => Boolean = recEq.tyFloat()(Set.empty)

  override def tyNat(): Exp[A] => Boolean = recEq.tyNat()(Set.empty)

  override def tyBool(): Exp[A] => Boolean = recEq.tyBool()(Set.empty)

  override def tyRecord(l: List[(String, Exp[A])]): Exp[A] => Boolean = recEq.tyRecord(l)(Set.empty)

  override def tyString(): Exp[A] => Boolean = recEq.tyString()(Set.empty)

  override def tyVar(x: String): Exp[A] => Boolean = _ => false
}

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean] {
  override val recEq = RecEq
}

trait RecEq[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Set[(Exp[A], Exp[A])] => Exp[A] => Boolean]
  with typed.RecEq[A] with rectype.RecEq[A] {

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

  override def tyVar(x: String): T = _ => _ => false
}

object RecEq extends RecEq[TAlg] with TImpl[Set[(Exp[TAlg], Exp[TAlg])] => Exp[TAlg] => Boolean] {
  override val subst: (String, Exp[TAlg]) => TAlg[Exp[TAlg], Exp[TAlg]] =
    (x, e) => new TSubstImpl(x, e)
}

trait TSubst[A[-X, Y] <: TAlg[X, Y]] extends TTransform[A] with rectype.TSubst[A] with typevar.TSubst[A]

class TSubstImpl(_x: String, _e: Exp[TAlg]) extends TSubst[TAlg] with TImpl[Exp[TAlg]] {
  override val x: String = _x
  override val e: Exp[TAlg] = _e
}
