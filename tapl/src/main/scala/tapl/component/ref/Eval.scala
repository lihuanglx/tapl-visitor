package tapl.component.ref

import tapl.common._
import scala.collection.mutable
import tapl.component.unit
import tapl.component.unit.Alg.Factory._
import tapl.component.ref.Alg.Query
import tapl.component.ref.Alg.Factory._

trait Eval[A[-R, E] <: Alg[R, E] with unit.Alg[R, E]]
  extends Alg[Exp[A], mutable.MutableList[Exp[A]] => Exp[A]] with IIsVal[A] {

  override def tmAssign(l: Exp[A], r: Exp[A]): (mutable.MutableList[Exp[A]]) => Exp[A] = c =>
    if (l(isVal))
      if (r(isVal)) l match {
        case TmLoc(i) =>
          c.update(i, r)
          TmUnit[A]()
        case _ => typeError()
      }
      else TmAssign(l, apply(r)(c))
    else TmAssign(apply(l)(c), r)

  override def tmDeRef(e: Exp[A]): (mutable.MutableList[Exp[A]]) => Exp[A] = c =>
    if (e(isVal)) e match {
      case TmLoc(i) => c(i)
      case _ => typeError()
    }
    else TmDeRef(apply(e)(c))

  override def tmLoc(i: Int): (mutable.MutableList[Exp[A]]) => Exp[A] = _ => TmLoc[A](i)

  override def tmRef(e: Exp[A]): (mutable.MutableList[Exp[A]]) => Exp[A] = c =>
    if (e(isVal)) {
      c += e
      TmLoc(c.length - 1)
    } else TmRef(apply(e)(c))
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def tmLoc(i: Int): Boolean = true
}
