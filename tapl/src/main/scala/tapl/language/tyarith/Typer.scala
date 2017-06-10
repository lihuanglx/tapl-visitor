package tapl.language.tyarith

import tapl.common._
import tapl.component.{top, typedbool, typednat}

trait Typer[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] with typedbool.Typer[A, B] with typednat.Typer[A, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: Exp[TAlg] => Exp[TAlg] => Boolean =
    _ (new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean])
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typedbool.TEquals[A] with typednat.TEquals[A]

trait Typer2[A[-X, Y] <: Alg[X, Y], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[Exp[A], Type[B]] with typedbool.Typer2[A, B] with typednat.Typer[A, B]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typedbool.SubtypeOf[A] with typednat.SubtypeOf[A]

trait Join[A[-X, Y] <: TAlg[X, Y] with top.TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with typedbool.Join[A] with typednat.Join[A]
