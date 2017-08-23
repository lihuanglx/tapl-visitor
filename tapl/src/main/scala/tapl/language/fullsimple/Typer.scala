package tapl.language.fullsimple

import tapl.common._
import tapl.component.{typed, extension, variant}

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]] extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]]
  with typed.Typer[A, B] with extension.Typer[A, B] with variant.Typer[A, B]

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with extension.TEquals[A] with variant.TEquals[A]

trait TSubst[A[-X, Y] <: Type[X, Y]] extends Type.Transform[A] with typed.TSubst[A]
