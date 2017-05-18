package tapl.language.bot

import tapl.common.Util._
import tapl.common.{Exp, TyperSub}
import tapl.component.{topbot, typed}
import tapl.language.bot.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer[A, B] with TyperSub[B] {

  override def TmApp(e1: E3[A, Exp[B]], e2: E3[A, Exp[B]]): Type[B] = c => {
    apply(e1)(c) match {
      case CTyArr(t1, t2) => if (apply(e2)(c)(subtypeOf)(t1)) t2 else typeError("No subtyping relationship")
      case _ => typeError()
    }
  }
}

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

  override val subtypeOf: TAlg[Exp[TAlg], (Exp[TAlg]) => Boolean] =
    new SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with topbot.TEquals[A]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with topbot.SubtypeOf[A] with typed.SubtypeOf[A]
