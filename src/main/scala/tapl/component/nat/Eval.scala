package tapl.component.nat

import tapl.common.Util._
import tapl.common.{EvalAuxiliary, Exp}
import tapl.component.bool


trait Eval[A[-X, Y] <: Alg[X, Y] with bool.Alg[X, Y], M[_]]
  extends Alg[Exp[A], M[Exp[A]]] with EvalAuxiliary[A, M] {

  //val f: Factory[A] with bool.Factory[A]
  val f: A[Exp[A], Exp[A]]

  val isNumVal: A[Exp[A], Option[Int]]

  override def TmZero(): M[Exp[A]] = m.point(f.TmZero())

  override def TmPred(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      e(isNumVal).getOrElse(typeError())
      m.point(f.TmPred(e))
    } else {
      m.bind(apply(e))(x => m.point(f.TmPred(x)))
    }

  override def TmSucc(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      e(isNumVal).getOrElse(typeError())
      m.point(f.TmSucc(e))
    } else {
      m.bind(apply(e))(x => m.point(f.TmSucc(x)))
    }

  override def TmIsZero(e: Exp[A]): M[Exp[A]] =
    if (e(isVal)) {
      val x = e(isNumVal).getOrElse(typeError())
      m.point(if (x == 0) f.TmTrue() else f.TmFalse())
    } else {
      m.bind(apply(e))(x => m.point(f.TmIsZero(x)))
    }
}


trait IsVal[A[-R, _]] extends Query[A, Boolean] {
  override val default: Boolean = false

  override def TmZero(): Boolean = true

  override def TmPred(e: Exp[A]): Boolean = apply(e)

  override def TmSucc(e: Exp[A]): Boolean = apply(e)
}


trait IsNumVal[A[-R, _]] extends Query[A, Option[Int]] {
  override val default: Option[Int] = None

  override def TmZero(): Option[Int] = Some(0)

  override def TmPred(e: Exp[A]): Option[Int] = for {
    x <- apply(e)
  } yield x - 1

  override def TmSucc(e: Exp[A]): Option[Int] = for {
    x <- apply(e)
  } yield x + 1
}