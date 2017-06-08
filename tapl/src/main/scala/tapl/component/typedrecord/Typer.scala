package tapl.component.typedrecord

import tapl.common._
import tapl.component.top.CTyTop
import tapl.component.topbot.CTyBot
import tapl.component.{top, topbot}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Type[B]] {
  override def TmRecord(l: List[(String, Exp[A])]): Type[B] = c =>
    CTyRecord[B](l.map(x => (x._1, apply(x._2)(c))))

  override def TmProj(e: Exp[A], x: String): Type[B] = c =>
    apply(e)(c) match {
      case CTyRecord(l) => l.find(_._1 == x).getOrElse(typeError())._2
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyRecord(l: List[(String, Exp[A])]): (Exp[A]) => Boolean = {
    case CTyRecord(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(t2) && b
        case _ => false
      }
    })
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def TyRecord(l: List[(String, Exp[A])]): (Exp[A]) => Boolean = {
    case CTyRecord(l2) => l2.foldRight(true)({
      case ((n, t), b) => l.find(_._1 == n) match {
        case Some((_, u)) => apply(u)(t) && b
        case _ => false
      }
    })
    case CTyTop() => true
    case _ => false
  }
}

trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y] with topbot.TAlg[X, Y]] extends Typer[A, B] {
  override def TmProj(e: Exp[A], x: String): Type[B] = c =>
    apply(e)(c) match {
      case CTyBot() => CTyBot[B]()
      case _ => super.TmProj(e, x)(c)
    }
}

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def TyRecord(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u =>
    directJoin(CTyRecord[A](l), u).getOrElse(u match {
      case CTyRecord(l2) =>
        CTyRecord[A](l.foldRight(List[(String, Exp[A])]())({
          case ((n, t), r) => l2.find(_._1 == n) match {
            case Some(b) => (n, apply(t)(b._2)) :: r
            case _ => r
          }
        }))
      case _ => CTyTop[A]()
    })
}