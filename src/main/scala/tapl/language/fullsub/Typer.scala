package tapl.language.fullsub

import tapl.common._
import tapl.component.{simple, top}
import tapl.language.fullsub.TFactory._

trait Typer[A[-R, E, -F] <: Alg[R, E, F], B[-X, Y] <: TAlg[X, Y]]
  extends Alg[E3[A, Exp[B]], Type[B], Exp[B]] with simple.Typer2[A, B]

object Typer extends Typer[Alg, TAlg] with Impl[Type[TAlg]] {
  override val tEquals: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = TEquals

  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Join
}

trait TEquals[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with simple.TEquals[A] with top.TEquals[A]

object TEquals extends TEquals[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait SubtypeOf[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Boolean]
  with simple.SubtypeOf[A] with top.SubtypeOf[A]

object SubtypeOf extends SubtypeOf[TAlg] with TImpl[Exp[TAlg] => Boolean]

trait Join[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]]
  with simple.Join[A] with top.Join[A]

object Join extends Join[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val meet: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Meet
}

trait Meet[A[-X, Y] <: TAlg[X, Y]] extends TAlg[Exp[A], Exp[A] => Exp[A]] with top.Meet[A] {
  override def TyBool(): Exp[A] => Exp[A] = directMeet(CTyBool[A](), _).getOrElse(typeError())

  override def TyArr(t1: Exp[A], t2: Exp[A]): Exp[A] => Exp[A] = u =>
    directMeet(CTyArr[A](t1, t2), u).getOrElse(u match {
      case CTyArr(t3, t4) => CTyArr[A](t1(join)(t3), apply(t2)(t4))
      case _ => typeError()
    })

  override def TyUnit(): Exp[A] => Exp[A] = directMeet(CTyUnit[A](), _).getOrElse(typeError())

  override def TyFloat(): Exp[A] => Exp[A] = directMeet(CTyFloat[A](), _).getOrElse(typeError())

  override def TyNat(): Exp[A] => Exp[A] = directMeet(CTyNat[A](), _).getOrElse(typeError())

  override def TyRecord(l: List[(String, Exp[A])]): Exp[A] => Exp[A] = u =>
    directMeet(CTyRecord[A](l), u).getOrElse(u match {
      case CTyRecord(l2) =>
        val o1 = l.filter(b => l2.forall(_._1 != b._1))
        val o2 = l2.filter(b => l.forall(_._1 != b._1))
        val i = for {
          (n, t) <- l
          p = l2.find(_._1 == n)
          if p.nonEmpty
        } yield (n, apply(t)(p.get._2))
        CTyRecord[A](o1 ++ o2 ++ i)
      case _ => typeError()
    })

  override def TyString(): Exp[A] => Exp[A] = directMeet(CTyString[A](), _).getOrElse(typeError())

  override def TyVar(x: String): Exp[A] => Exp[A] = directMeet(CTyVar[A](x), _).getOrElse(typeError())
}

object Meet extends Meet[TAlg] with TImpl[Exp[TAlg] => Exp[TAlg]] {
  override val subtypeOf: TAlg[Exp[TAlg], Exp[TAlg] => Boolean] = SubtypeOf

  override val join: TAlg[Exp[TAlg], Exp[TAlg] => Exp[TAlg]] = Join
}
