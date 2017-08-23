package tapl.component.variant

import tapl.common._
import tapl.component.variant.Type.Factory._
import tapl.component.top.Type.Factory._
import tapl.component.top

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with ITEq[B] {

  override def tmTag(x: String, e: Exp2[A, Exp[B]], t: Exp[B]): CtxTo[B] = c =>
    t match {
      case TyVariant(l) =>
        val t2 = l.find(_._1 == x).getOrElse(typeError())._2
        if (tEquals(apply(e)(c))(t2)) t else typeError()
      case _ => typeError()
    }

  override def tmCase(e: Exp2[A, Exp[B]], l: List[(String, String, Exp2[A, Exp[B]])]): CtxTo[B] = c =>
    apply(e)(c) match {
      case TyVariant(l2) =>
        l.map({ case (n, v, b) =>
          val t = l2.find(_._1 == n).getOrElse(typeError())._2
          apply(b)(c + (v, t))
        }).reduce((x, y) => if (tEquals(x)(y)) x else typeError())
      case _ => typeError()
    }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyVariant(l: List[(String, Exp[A])]): Exp[A] => Boolean = {
    case TyVariant(l2) => l.length == l2.length && l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(t2) && b
        case _ => false
      }
    })
    case _ => false
  }
}

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] {
  override def tyVariant(l: List[(String, Exp[A])]): Exp[A] => Boolean = {
    case TyVariant(l2) => l.foldRight(true)({
      case ((n, t), b) => l2.find(_._1 == n) match {
        case Some((_, t2)) => apply(t)(t2) && b
        case _ => false
      }
    })
    case TyTop() => true
    case _ => false
  }
}

trait Join[A[-X, Y] <: Type[X, Y] with top.Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with JoinAux[A] {
  override def tyVariant(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u => {
    directJoin(TyVariant[A](l), u).getOrElse(u match {
      case TyVariant(l2) =>
        TyVariant[A](l.foldRight(List[(String, Exp[A])]())({
          case ((n, t), r) => l2.find(_._1 == n) match {
            case Some(b) => (n, apply(t)(b._2)) :: r
            case _ => r
          }
        }))
      case _ => TyTop[A]()
    })
  }
}
