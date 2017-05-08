package tapl.component.record

import tapl.common.Util._
import tapl.common.{EvalAuxiliary, Exp}
import tapl.component.record.Factory._

import scalaz.Scalaz._

trait Eval[A[-X, Y] <: Alg[X, Y], M[_]] extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {
  override def TmRecord(l: List[(String, Exp[A])]): M[Exp[A]] = {
    val (vs, es) = l.partition(x => isVal(x._2))
    es match {
      case (x, e) :: xs => for {
        _e <- apply(e)
      } yield CRecord(vs ++ ((x, _e) :: xs))
      case Nil => m.point(CRecord(l))
    }
  }

  override def TmProj(e: Exp[A], x: String): M[Exp[A]] =
    if (e(isVal)) {
      val r = e match {
        case CRecord(l) => l.find(_._1 == x).getOrElse(typeError())._2
        case _ => typeError()
      }
      m.point(r)
    } else for {
      _e <- apply(e)
    } yield CProj(_e, x)
}

trait IsVal[A[-R, _]] extends Query[Exp[A], Boolean] {
  override val default: Boolean = false

  override def TmRecord(l: List[(String, Exp[A])]): Boolean = l.forall(x => apply(x._2))
}