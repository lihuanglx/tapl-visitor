package language
package boolstlc

import gems._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], List[(String, Exp[B])] => Exp[B], Exp[B]]
    with bool.Term.Lifter[Exp2[A, Exp[B]], Exp[B], List[(String, Exp[B])]]
    with stlc.Typer[A, B] with Type.AllConvertChains[B] {

  def propagate(c: List[(String, Exp[B])]): bool.Term[Exp2[A, Exp[B]], Exp[B]] =
    new bool.Typer[A[-?, ?, Exp[B]], B] {
      def tEquals: B[Exp[B], (Exp[B]) => Boolean] = Typer.this.tEquals

      def convertBool[C[-R, _]](e: SExp[B, C]): Option[SExp[bool.Type, C]] =
        Typer.this.convertBool(e)

      def apply(e: Exp2[A, Exp[B]]): Exp[B] = Typer.this.apply(e)(c)
    }
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with bool.TEquals[A] with stlc.TEquals[A] with Type.AllConvertChains[A]
