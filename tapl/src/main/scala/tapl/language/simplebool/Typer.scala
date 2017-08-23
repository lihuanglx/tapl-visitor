package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]]
  with typed.Typer[A, B] with typedbool.Term.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] {

  override def propagate(c: Ctx[String, Exp[B]]): typedbool.Term[Exp2[A, Exp[B]], Exp[B]] =
    new typedbool.Typer[A[-?, ?, Exp[B]], B] {
      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] = Typer.this.apply(e)(c)

      override val tEquals: Exp[B] => Exp[B] => Boolean = Typer.this.tEquals
    }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with typedbool.TEquals[A]
