package tapl.language.bot

import tapl.common._
import tapl.component.{top, bottom, typed}
import tapl.language.bot.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with typed.Typer2[A, B] {

  override def tmApp(e1: Exp2[A, Exp[B]], e2: Exp2[A, Exp[B]]): CtxTo[B] = c =>
    apply(e1)(c) match {
      case TyBot() => TyBot[B]()
      case _ => super.tmApp(e1, e2)(c)
    }
}

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean =
    _ (new TEquals[Type] with TImpl[Exp[Type] => Boolean])

  override val subtypeOf: Type[Exp[Type], (Exp[Type]) => Boolean] =
    new SubtypeOf[Type] with TImpl[Exp[Type] => Boolean]
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with top.TEquals[A] with bottom.TEquals[A]

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with top.SubtypeOf[A] with bottom.SubtypeOf[A] with typed.SubtypeOf[A]

trait Join[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]]
  with typed.Join[A] with top.Join[A] with bottom.Join[A]

trait Meet[A[-X, Y] <: Type[X, Y]]
  extends Type[Exp[A], Exp[A] => Exp[A]] with top.Meet[A] with bottom.Meet[A] {

  override def tyArr(t1: Exp[A], t2: Exp[A]): (Exp[A]) => Exp[A] = u =>
    directMeet(TyArr[A](t1, t2), u).getOrElse(u match {
      case TyArr(t3, t4) => TyArr[A](t1(join)(t3), apply(t2)(t4))
      case _ => TyBot[A]()
    })
}
