package tapl.component.typedrecord

import tapl.common.Util._
import tapl.common.{Exp, TyperAux}
import tapl.component.typedrecord.TFactory._

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp[A], Type[B]] with TyperAux[B] {
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
