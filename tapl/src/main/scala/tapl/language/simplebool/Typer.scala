package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp2[A, Exp[B]], Type[B], Exp[B]]
  with typed.Typer[A, B] with typedbool.Alg.Lifter[Exp2[A, Exp[B]], Exp[B], Ctx[String, Exp[B]]] {

  override def go(c: Ctx[String, Exp[B]]): typedbool.Alg[Exp2[A, Exp[B]], Exp[B]] =
    new typedbool.Typer[A[-?, ?, Exp[B]], B] {
      override def apply(e: Exp[A[-?, ?, Exp[B]]]): Exp[B] = Typer.this.apply(e)(c)

      override val tEquals: Exp[B] => Exp[B] => Boolean = Typer.this.tEquals
    }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean =
    _ (new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean])
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with typedbool.TEquals[A]
