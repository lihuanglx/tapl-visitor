package language
package stlc

import gems._
import Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], List[(String, Exp[B])] => Exp[B], Exp[B]]
    with Type.Convert[B] with ITEquals[B] {

  def tmAbs(x: String, t: Exp[B], e: Exp2[A, Exp[B]]): (List[(String, Exp[B])]) => Exp[B] =
    c => {
      val t2 = apply(e)((x, t) +: c)
      TyArr[B, B](t, t2)
    }

  def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): (List[(String, Exp[B])]) => Exp[B] =
    c => {
      val t1 = convertStlc(apply(e1)(c)).getOrElse(cnvFailed)
      val t2 = apply(e2)(c)
      t1(new Type.Query[Exp[B], Exp[B]] {
        def default: Exp[B] = typeError

        override def tyArr(u1: Exp[B], u2: Exp[B]): Exp[B] =
          if (tEquals(u1)(t2)) u2 else typeError

        def apply(t: Exp[B]): Exp[B] = impossible
      })
    }

  def tmUnit(): (List[(String, Exp[B])]) => Exp[B] =
    c => TyUnit[B, B]()

  def tmVar(x: String): (List[(String, Exp[B])]) => Exp[B] =
    c => {
      val i = c.indexWhere(_._1 == x)
      if (i == -1) typeError else c(i)._2
    }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] with Type.Convert[A] {
  def tyUnit(): Exp[A] => Boolean = t => {
    val c = convertStlc(t).getOrElse(cnvFailed)
    c(new Type.Query[Exp[A], Boolean] {
      def default: Boolean = false

      override def tyUnit(): Boolean = true

      def apply(t: Exp[A]): Boolean = impossible
    })
  }

  def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Boolean = t => {
    val c = convertStlc(t).getOrElse(cnvFailed)
    c(new Type.Query[Exp[A], Boolean] {
      def default: Boolean = false

      override def tyArr(u1: Exp[A], u2: Exp[A]): Boolean =
        TEquals.this.apply(t1)(u1) && TEquals.this.apply(t2)(u2)

      def apply(t: Exp[A]): Boolean = impossible
    })
  }
}
