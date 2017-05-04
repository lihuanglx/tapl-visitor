package tapl.component.typedbool

import tapl.component.bool
import tapl.common.Exp

trait Alg[-RT, T] {
  def TyBool(): T

  def TyArr(t1: RT, t2: RT): T
}

trait Alg2[-RE, E, -RT, T] {
  def Lam(x: String, t: RT, e: RE): E
}


// trait Factory[V[-A, B, -C, D] <: Alg[A, B, C, D]] extends Alg[Exp[?], Exp[?]] {}
/*
object TypedBool {

  trait Alg[E, T] extends Bool.Alg[E] {
    def TyBool(): T
  }

  trait Print extends Alg[String, String] with Bool.Print {
    def TyBool() = "Bool"
  }

  trait Parse[E, T] extends Bool.Parse[E] {
    lexical.reserved += "Bool"

    override val alg: Alg[E, T]

    val pTypedBoolE: Parser[E] = pBoolE
    val pTypedBoolT: Parser[T] = "Bool" ^^ { _ => alg.TyBool() }
  }

}
 */

/*
trait Alg[-R, E] {
  def TmTrue(): E

  def TmFalse(): E

  def TmIf(e1: R, e2: R, e3: R): E

  def apply(e: R): E
}

trait Factory[A[-X, Y] <: Alg[X, Y]] extends Alg[Exp[A], Exp[A]] {

  override def TmTrue(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmTrue()
  }

  override def TmFalse(): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmFalse()
  }

  override def TmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[A] = new Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.TmIf(e1, e2, e3)
  }

}
 */