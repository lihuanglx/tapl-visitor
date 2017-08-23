package tapl.language.fullsub

import tapl.common._
import tapl.component.{typed, extension, top}
import tapl.language.fullsub.Type.Factory._

trait Typer[A[-R, E, -F] <: Term[R, E, F], B[-X, Y] <: Type[X, Y]]
  extends Term[Exp2[A, Exp[B]], CtxTo[B], Exp[B]] with typed.Typer2[A, B] with extension.Typer2[A, B]

object Typer extends Typer[Term, Type] with Impl[CtxTo[Type]] {
  override val tEquals: Exp[Type] => Exp[Type] => Boolean = _ (TEquals)

  override val subtypeOf: Type[Exp[Type], Exp[Type] => Boolean] = SubtypeOf

  override val join: Type[Exp[Type], Exp[Type] => Exp[Type]] = Join
}

trait TEquals[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typed.TEquals[A] with extension.TEquals[A] with top.TEquals[A]

object TEquals extends TEquals[Type] with TImpl[Exp[Type] => Boolean]

trait SubtypeOf[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Boolean]
  with typed.SubtypeOf[A] with extension.SubtypeOf[A] with top.SubtypeOf[A]

object SubtypeOf extends SubtypeOf[Type] with TImpl[Exp[Type] => Boolean]

trait Join[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]]
  with typed.Join[A] with extension.Join[A] with top.Join[A]

object Join extends Join[Type] with TImpl[Exp[Type] => Exp[Type]] {
  override val subtypeOf: Type[Exp[Type], Exp[Type] => Boolean] = SubtypeOf

  override val meet: Type[Exp[Type], Exp[Type] => Exp[Type]] = Meet
}

trait Meet[A[-X, Y] <: Type[X, Y]] extends Type[Exp[A], Exp[A] => Exp[A]] with top.Meet[A] with Default[Exp[A]] {
  override lazy val default: Exp[A] = typeError()

  override def tyBool(): Exp[A] => Exp[A] = directMeet(TyBool[A](), _).getOrElse(default)

  override def tyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Exp[A] = u =>
    directMeet(TyArr[A](t1, t2), u).getOrElse(u match {
      case TyArr(t3, t4) => TyArr[A](t1(join)(t3), apply(t2)(t4))
      case _ => default
    })

  override def tyUnit(): Exp[A] => Exp[A] = directMeet(TyUnit[A](), _).getOrElse(default)

  override def tyFloat(): Exp[A] => Exp[A] = directMeet(TyFloat[A](), _).getOrElse(default)

  override def tyNat(): Exp[A] => Exp[A] = directMeet(TyNat[A](), _).getOrElse(default)

  override def tyRecord(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u =>
    directMeet(TyRecord[A](l), u).getOrElse(u match {
      case TyRecord(l2) =>
        val o1 = l.filter(b => l2.forall(_._1 != b._1))
        val o2 = l2.filter(b => l.forall(_._1 != b._1))
        val i = for {
          (n, t) <- l
          p = l2.find(_._1 == n)
          if p.nonEmpty
        } yield (n, apply(t)(p.get._2))
        TyRecord[A](o1 ++ o2 ++ i)
      case _ => default
    })

  override def tyString(): Exp[A] => Exp[A] = directMeet(TyString[A](), _).getOrElse(default)

  override def tyVar(x: String): Exp[A] => Exp[A] = directMeet(TyVar[A](x), _).getOrElse(default)
}

object Meet extends Meet[Type] with TImpl[Exp[Type] => Exp[Type]] {
  override val subtypeOf: Type[Exp[Type], Exp[Type] => Boolean] = SubtypeOf

  override val join: Type[Exp[Type], Exp[Type] => Exp[Type]] = Join
}
