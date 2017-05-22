package tapl.language.bot

import tapl.common._
import tapl.component.{topbot, typed}
import tapl.language.bot.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with typed.Typer3[A, B]

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

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with typed.Join[A] with topbot.Join[A]

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with topbot.Meet[A] {
  override def TyArr(t1: Exp[A], t2: Exp[A]): (Exp[A]) => Exp[A] = u =>
    directMeet(CTyArr[A](t1, t2), u).getOrElse(u match {
      case CTyArr(t3, t4) => CTyArr[A](t1(join)(t3), apply(t2)(t4))
      case _ => CTyBot[A]()
    })
}
