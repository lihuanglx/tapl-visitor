package tapl.component.record

import tapl.common._
import tapl.component.record.Alg.Query
import tapl.component.record.Alg.Factory._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with IIsVal[A] {
  override def tmRecord(l: List[(String, Exp[A])]): Exp[A] = {
    val (vs, es) = l.partition(x => isVal(x._2))
    es match {
      case (x, e) :: xs => TmRecord(vs ++ ((x, apply(e)) :: xs))
      case Nil => TmRecord(l)
    }
  }

  override def tmProj(e: Exp[A], x: String): Exp[A] =
    if (e(isVal)) {
      e match {
        case TmRecord(l) => l.find(_._1 == x).getOrElse(typeError())._2
        case _ => typeError()
      }
    } else {
      TmProj(apply(e), x)
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def tmRecord(l: List[(String, Exp[A])]): Boolean = l.forall(x => apply(x._2))
}
