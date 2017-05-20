package tapl.component.record

import tapl.common._

trait Eval[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] with EvalAux[A] {
  override def TmRecord(l: List[(String, Exp[A])]): Exp[A] = {
    val (vs, es) = l.partition(x => isVal(x._2))
    es match {
      case (x, e) :: xs => CRecord(vs ++ ((x, apply(e)) :: xs))
      case Nil => CRecord(l)
    }
  }

  override def TmProj(e: Exp[A], x: String): Exp[A] =
    if (e(isVal)) {
      e match {
        case CRecord(l) => l.find(_._1 == x).getOrElse(typeError())._2
        case _ => typeError()
      }
    } else {
      CProj(apply(e), x)
    }
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmRecord(l: List[(String, Exp[A])]): Boolean = l.forall(x => apply(x._2))
}