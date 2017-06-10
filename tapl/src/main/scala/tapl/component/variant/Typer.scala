package tapl.component.variant

import tapl.common._
import tapl.component.variant.TAlg.Factory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]] with ITEq[B] {

  override def tmTag(x: String, e: TExp[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    t match {
      case TyVariant(l) =>
        val t2 = l.find(_._1 == x).getOrElse(typeError())._2
        if (tEquals(apply(e)(c))(t2)) t else typeError()
      case _ => typeError()
    }

  override def tmCase(e: TExp[A, Exp[B]], l: List[(String, String, TExp[A, Exp[B]])]): Type[B] = c =>
    apply(e)(c) match {
      case TyVariant(l2) =>
        l.map({ case (n, v, b) =>
          val t = l2.find(_._1 == n).getOrElse(typeError())._2
          apply(b)(c + (v, t))
        }).reduce((x, y) => if (tEquals(x)(y)) x else typeError())
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean] {
  override def tyVariant(l: List[(String, Exp[A])]): (Exp[A]) => Boolean = {
    case TyVariant(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(t2) && b
        case _ => false
      }
    })
    case _ => false
  }
}
