package tapl.component.variant

import tapl.common.{Exp, TyperAux}
import tapl.common.Util._
import tapl.component.variant.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with TyperAux[B] {

  override def TmTag(x: String, e: E3[A, Exp[B]], t: Exp[B]): Type[B] = c =>
    t match {
      case CTyVariant(l) =>
        val t2 = l.find(_._1 == x).getOrElse(typeError())._2
        if (apply(e)(c)(tEquals)(t2)) t else typeError()
      case _ => typeError()
    }

  override def TmCase(e: E3[A, Exp[B]], l: List[(String, String, E3[A, Exp[B]])]): Type[B] = c =>
    apply(e)(c) match {
      case CTyVariant(l2) =>
        l.map({ case (n, v, b) =>
          val t = l2.find(_._1 == n).getOrElse(typeError())._2
          apply(b)(c + (v, t))
        }).reduce((x, y) => if (x(tEquals)(y)) x else typeError())
      case _ => typeError()
    }
}
