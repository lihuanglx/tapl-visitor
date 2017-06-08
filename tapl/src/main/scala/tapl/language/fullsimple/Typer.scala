package tapl.language.fullsimple

import tapl.common._
import tapl.component.{simple, variant}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[TExp[A, Exp[B]], Type[B], Exp[B]]
  with simple.Typer[A, B] with variant.Typer[A, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with simple.TEquals[A] with variant.TEquals[A]
