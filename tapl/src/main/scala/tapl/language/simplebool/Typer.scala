package tapl.language.simplebool

import tapl.common._
import tapl.component.{typed, typedbool}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[E3[A, Exp[B]], Type[B], Exp[B]]
  with typed.Typer[A, B] with typedbool.Typer[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with typedbool.TEquals[A]
