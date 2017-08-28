package convert.component.record

import convert.common._
import convert.component.record.Term.Query
import convert.component.record.Term.Factory._

trait Eval[A[-X, Y] <: Term[X, Y]] extends Term[Exp[A], Exp[A]] with IIsVal[A] with Term.Convert[A] {
  override def tmRecord(l: List[(String, Exp[A])]): Exp[A] = {
    val (vs, es) = l.partition(x => isVal(x._2))
    es match {
      case (x, e) :: xs => TmRecord(vs ++ ((x, apply(e)) :: xs))
      case Nil => TmRecord(l)
    }
  }

  override def tmProj(e: Exp[A], x: String): Exp[A] =
    if (e(isVal)) {
      convertRecord(e) match {
        case Some(t) => t(new Query[Exp[A], Exp[A]] {
          override def default: Exp[A] = typeError()

          override def tmRecord(l: List[(String, Exp[A])]): Exp[A] = l.find(_._1 == x).getOrElse(typeError())._2

          override def apply(e: Exp[A]): Exp[A] = impossible
        })
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
