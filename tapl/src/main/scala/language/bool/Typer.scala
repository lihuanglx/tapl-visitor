package language
package bool

import gems._
import Type.Factory._

trait Typer[A[-X, Y] <: Term[X, Y], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp[A], Exp[B]] with Type.Convert[B] with ITEquals[B] {

  def tmTrue(): Exp[B] = TyBool[B, B]()

  def tmFalse(): Exp[B] = TyBool[B, B]()

  def tmIf(e1: Exp[A], e2: Exp[A], e3: Exp[A]): Exp[B] = {
    val c = convertBool(apply(e1)).getOrElse(cnvFailed)
    c(new Type.Query[Exp[B], Exp[B]] {
      def default: Exp[B] = typeError

      override def tyBool(): Exp[B] = {
        val t2 = Typer.this.apply(e2)
        val t3 = Typer.this.apply(e3)
        if (t2(tEquals)(t3)) t2 else typeError
      }

      def apply(e: Exp[B]): Exp[B] = impossible
    })
  }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean] with Type.Convert[A] {
  def tyBool(): Exp[A] => Boolean = t => {
    val c = convertBool(t).getOrElse(cnvFailed)
    c(new Type.Query[Exp[A], Boolean] {
      def default: Boolean = false

      override def tyBool(): Boolean = true

      def apply(e: Exp[A]): Boolean = impossible
    })
  }
}
