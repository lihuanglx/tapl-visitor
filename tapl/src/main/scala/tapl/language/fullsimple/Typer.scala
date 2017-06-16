package tapl.language.fullsimple

import tapl.common._
import tapl.component.{typed, extension, variant}

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]] extends Alg[Exp2[A, Exp[B]], Type[B], Exp[B]]
  with typed.Typer[A, B] with extension.Typer[A, B] with variant.Typer[A, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean =
    _ (new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean])
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with extension.TEquals[A] with variant.TEquals[A]
