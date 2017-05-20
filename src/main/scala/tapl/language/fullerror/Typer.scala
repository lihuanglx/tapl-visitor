package tapl.language.fullerror

import tapl.common.Exp
import tapl.common.Util._
import tapl.component.{typedbool, typevar}
import tapl.language.bot
import tapl.language.fullerror.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with bot.Typer[A, B]
    with typedbool.Typer2[({type lam[-X, Y] = A[X, Y, Exp[B]]})#lam, B] {

  override def TmError(): Type[B] = CTyBot[B]()

  override def TmTry(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c =>
    join(apply(e1)(c), apply(e2)(c))
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

  override val subtypeOf: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with bot.TEquals[A] with typedbool.TEquals[A] with typevar.TEquals[A]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with bot.SubtypeOf[A] with typevar.SubtypeOf[A] {

  override def TyBool(): (Exp[A]) => Boolean = {
    case CTyTop() => true
    case CTyBool() => true
    case _ => false
  }
}
