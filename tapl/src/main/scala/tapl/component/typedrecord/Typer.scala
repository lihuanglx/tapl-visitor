package tapl.component.typedrecord

import tapl.common._
import tapl.component.top.TAlg.Factory._
import tapl.component.bottom.TAlg.Factory._
import tapl.component.{top, bottom}
import tapl.component.typedrecord.TAlg.Factory._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Exp[B]] {
  override def tmRecord(l: List[(String, Exp[A])]): Exp[B] =
    TyRecord[B](l.map(x => (x._1, apply(x._2))))

  override def tmProj(e: Exp[A], x: String): Exp[B] =
    apply(e) match {
      case TyRecord(l) => l.find(_._1 == x).getOrElse(typeError())._2
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyRecord(l: List[(String, Exp[A])]): (Exp[A]) => Boolean = {
    case TyRecord(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(t2) && b
        case _ => false
      }
    })
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyRecord(l: List[(String, Exp[A])]): (Exp[A]) => Boolean = {
    case TyRecord(l2) => l2.foldRight(true)({
      case ((n, t), b) => l.find(_._1 == n) match {
        case Some((_, u)) => apply(u)(t) && b
        case _ => false
      }
    })
    case TyTop() => true
    case _ => false
  }
}

trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with bottom.TAlg[X, Y]] extends Typer[A, B] {
  override def tmProj(e: Exp[A], x: String): Exp[B] =
    apply(e) match {
      case TyBot() => TyBot[B]()
      case _ => super.tmProj(e, x)
    }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyRecord(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u =>
    directJoin(TyRecord[A](l), u).getOrElse(u match {
      case TyRecord(l2) =>
        TyRecord[A](l.foldRight(List[(String, Exp[A])]())({
          case ((n, t), r) => l2.find(_._1 == n) match {
            case Some(b) => (n, apply(t)(b._2)) :: r
            case _ => r
          }
        }))
      case _ => TyTop[A]()
    })
}
