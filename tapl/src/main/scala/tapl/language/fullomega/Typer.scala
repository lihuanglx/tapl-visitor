package tapl.language.fullomega

import tapl.common._
import tapl.component._
import tapl.language.fullomega.TAlg.Factory._
import tapl.language.fullomega.KAlg.Factory._

object Wrapper {
  type TC[B[-X, Y, -Z], C[-X, Y]] = Ctx[String, _Exp2[B, C]]
  type RC[B[-X, Y, -Z], C[-X, Y]] = Ctx[Int, _Exp2[B, C]]
  type KC[C[-X, Y]] = Ctx[String, Exp[C]]
}

import Wrapper._

trait Typer0[A[-R, E, -T, -K] <: Alg[R, E, T, K], B[-X, Y, -Z] <: TAlg[X, Y, Z], C[-X, Y] <: KAlg[X, Y]]
  extends Alg[_Exp3[A, B, C], RC[B, C] => TC[B, C] => _Exp2[B, C], _Exp2[B, C], Exp[C]]
    with typed.Alg.Lifter[_Exp3[A, B, C], TC[B, C] => _Exp2[B, C], _Exp2[B, C], Ctx[Int, _Exp2[B, C]]]
    with extension.Alg.Lifter[_Exp3[A, B, C], TC[B, C] => _Exp2[B, C], _Exp2[B, C], Ctx[Int, _Exp2[B, C]]]
    with ref.Typer[({type l[-X, Y] = A[X, Y, _Exp2[B, C], Exp[C]]})#l, ({type l[-X, Y] = B[X, Y, Exp[C]]})#l] {

  override def go(c: Ctx[Int, _Exp2[B, C]]) =
    new typed.Typer[({type l[-X, Y, -Z] = A[X, Y, Z, Exp[C]]})#l, ({type l[-X, Y] = B[X, Y, Exp[C]]})#l]
      with extension.Typer[({type l[-X, Y, -Z] = A[X, Y, Z, Exp[C]]})#l, ({type l[-X, Y] = B[X, Y, Exp[C]]})#l] {

      override def apply(e: _Exp3[A, B, C]): TC[B, C] => _Exp2[B, C] = Typer0.this.apply(e)(c)

      override val tEquals: _Exp2[B, C] => _Exp2[B, C] => Boolean = Typer0.this.tEquals
    }
}

trait Typer[A[-R, E, -T, -K] <: Alg[R, E, T, K], B[-X, Y, -Z] <: TAlg[X, Y, Z], C[-X, Y] <: KAlg[X, Y]]
  extends Alg[_Exp3[A, B, C], KC[C] => RC[B, C] => TC[B, C] => _Exp2[B, C], _Exp2[B, C], Exp[C]]
    with Alg.Lifter[_Exp3[A, B, C], RC[B, C] => TC[B, C] => _Exp2[B, C], _Exp2[B, C], Exp[C], KC[C]]
    with ITEq[({type l[-X, Y] = B[X, Y, Exp[C]]})#l] with ISubst[({type l[-X, Y] = B[X, Y, Exp[C]]})#l] {

  override def go(c: KC[C]) = new Typer0[A, B, C] {
    override val tEquals: (_Exp2[B, C]) => (_Exp2[B, C]) => Boolean = Typer.this.tEquals

    override def apply(e: _Exp3[A, B, C]) = Typer.this.apply(e)(c)

    override def tmTAbs(x: String, k: Exp[C], e: _Exp3[A, B, C]): (RC[B, C]) => (TC[B, C]) => _Exp2[B, C] =
      Typer.this.tmTAbs(x, k, e)(c)

    override def tmTApp(e: _Exp3[A, B, C], t: _Exp2[B, C]): (RC[B, C]) => (TC[B, C]) => _Exp2[B, C] =
      Typer.this.tmTApp(e, t)(c)

    override def tmPack(t1: _Exp2[B, C], e: _Exp3[A, B, C], t2: _Exp2[B, C]): (RC[B, C]) => (TC[B, C]) => _Exp2[B, C] =
      Typer.this.tmPack(t1, e, t2)(c)

    override def tmUnpack(tx: String, x: String, e1: _Exp3[A, B, C], e2: _Exp3[A, B, C]) =
      Typer.this.tmUnpack(tx, x, e1, e2)(c)
  }

  val kinding: B[_Exp2[B, C], KC[C] => Exp[C], Exp[C]]

  val kEquals: Exp[C] => Exp[C] => Boolean

  val tEval: B[_Exp2[B, C], _Exp2[B, C], Exp[C]]

  override def tmTAbs(x: String, k: Exp[C], e: _Exp3[A, B, C]): (KC[C]) => (RC[B, C]) => (TC[B, C]) => _Exp2[B, C] =
    kc => rc => tc => {
      val t = apply(e)(kc + (x -> k))(rc)(tc)
      TyAll(x, k, t)
    }

  override def tmTApp(e: _Exp3[A, B, C], t: _Exp2[B, C]): (KC[C]) => (RC[B, C]) => (TC[B, C]) => _Exp2[B, C] =
    kc => rc => tc => apply(e)(kc)(rc)(tc) match {
      case TyAll(x, k, t1) if kEquals(k)(t(kinding)(kc)) => t1(subst(x, t))(tEval)
      case _ => typeError()
    }

  override def tmPack(t1: _Exp2[B, C], e: _Exp3[A, B, C], t2: _Exp2[B, C]) =
    kc => rc => tc => {
      val t = t2(tEval)
      t match {
        case TySome(x, k, b) if kEquals(k)(t1(kinding)(kc)) && tEquals(apply(e)(kc)(rc)(tc))(b(subst(x, t1))) => t
        case _ => typeError()
      }
    }

  override def tmUnpack(tx: String, x: String, e1: _Exp3[A, B, C], e2: _Exp3[A, B, C]) =
    kc => rc => tc => {
      apply(e1)(kc)(rc)(tc) match {
        case TySome(y, k, b) =>
          apply(e2)(kc + (tx -> k))(rc)(tc + (x -> b(subst(y, TyVar[({type l[-X, Y] = B[X, Y, Exp[C]]})#l](tx)))))
        case _ => typeError()
      }
    }
}

object Typer extends Typer[Alg, TAlg, KAlg]
  with Impl[KC[KAlg] => RC[TAlg, KAlg] => TC[TAlg, KAlg] => _Exp2[TAlg, KAlg]] {

  override val kEquals: (Exp[KAlg]) => (Exp[KAlg]) => Boolean = _ (KEquals)

  override val kinding: TAlg[_Exp2[TAlg, KAlg], KC[KAlg] => Exp[KAlg], Exp[KAlg]] = Kinding

  override def subst(m: Map[String, _Exp2[TAlg, KAlg]]): TAlg[_Exp2[TAlg, KAlg], _Exp2[TAlg, KAlg], Exp[KAlg]] =
    new TSubstImpl(m)

  override val tEval: TAlg[_Exp2[TAlg, KAlg], _Exp2[TAlg, KAlg], Exp[KAlg]] = TEval

  override val tEquals: (_Exp2[TAlg, KAlg]) => (_Exp2[TAlg, KAlg]) => Boolean =
    a => b => a(tEval)(TEquals)(Set.empty)(b(tEval))
}

trait TEquals[B[-X, Y, -Z] <: TAlg[X, Y, Z], C[-X, Y] <: KAlg[X, Y]]
  extends TAlg[_Exp2[B, C], Set[(String, String)] => _Exp2[B, C] => Boolean, Exp[C]]
    with typed.TAlg.Lifter[_Exp2[B, C], _Exp2[B, C] => Boolean, Set[(String, String)]]
    with extension.TAlg.Lifter[_Exp2[B, C], _Exp2[B, C] => Boolean, Set[(String, String)]]
    with ref.TAlg.Lifter[_Exp2[B, C], _Exp2[B, C] => Boolean, Set[(String, String)]] {

  override def go(c: Set[(String, String)]) =
    new typed.TEquals[({type l[-X, Y] = B[X, Y, Exp[C]]})#l]
      with extension.TEquals[({type l[-X, Y] = B[X, Y, Exp[C]]})#l]
      with ref.TEquals[({type l[-X, Y] = B[X, Y, Exp[C]]})#l] {

      override def apply(t: _Exp2[B, C]): (_Exp2[B, C]) => Boolean = TEquals.this.apply(t)(c)
    }

  val kEquals: Exp[C] => Exp[C] => Boolean

  override def tyAll(x: String, k: Exp[C], t: _Exp2[B, C]): (Set[(String, String)]) => (_Exp2[B, C]) => Boolean =
    c => {
      case TyAll(x2, k2, t2) if kEquals(k)(k2) => apply(t)(c + (x -> x2))(t2)
      case _ => false
    }

  override def tySome(x: String, k: Exp[C], t: _Exp2[B, C]): (Set[(String, String)]) => (_Exp2[B, C]) => Boolean =
    c => {
      case TySome(x2, k2, t2) if kEquals(k)(k2) => apply(t)(c + (x -> x2))(t2)
      case _ => false
    }

  override def tyAbs(x: String, k: Exp[C], t: _Exp2[B, C]): (Set[(String, String)]) => (_Exp2[B, C]) => Boolean =
    c => {
      case TyAbs(x2, k2, t2) if kEquals(k)(k2) => apply(t)(c + (x -> x2))(t2)
      case _ => false
    }

  override def tyApp(t1: _Exp2[B, C], t2: _Exp2[B, C]): (Set[(String, String)]) => (_Exp2[B, C]) => Boolean =
    c => {
      case TyApp(t11, t22) => apply(t1)(c)(t11) && apply(t2)(c)(t22)
      case _ => false
    }

  override def tyVar(x: String): Set[(String, String)] => (_Exp2[B, C]) => Boolean = c => {
    case TyVar(y) => (y == x) || c((x, y))
    case _ => false
  }
}

object TEquals extends TEquals[TAlg, KAlg] with TImpl[Set[(String, String)] => _Exp2[TAlg, KAlg] => Boolean] {
  override val kEquals: (Exp[KAlg]) => (Exp[KAlg]) => Boolean = _ (KEquals)
}

trait KEquals[C[-X, Y] <: KAlg[X, Y]] extends KAlg[Exp[C], Exp[C] => Boolean] {
  override def knStar(): (Exp[C]) => Boolean = {
    case KnStar() => true
    case _ => false
  }

  override def knArr(k1: Exp[C], k2: Exp[C]): (Exp[C]) => Boolean = {
    case KnArr(k11, k22) => apply(k1)(k11) && apply(k2)(k22)
    case _ => false
  }
}

object KEquals extends KEquals[KAlg] with KImpl[Exp[KAlg] => Boolean]

// todo: error
trait Kinding[B[-X, Y, -Z] <: TAlg[X, Y, Z], C[-X, Y] <: KAlg[X, Y]]
  extends TAlg[_Exp2[B, C], (KC[C]) => Exp[C], Exp[C]] {

  val kEquals: Exp[C] => Exp[C] => Boolean

  override def tyArr(t1: _Exp2[B, C], t2: _Exp2[B, C]): (KC[C]) => Exp[C] = c =>
    (apply(t1)(c), apply(t2)(c)) match {
      case (KnStar(), KnStar()) => KnStar()
      case _ => typeError()
    }

  override def tyBool(): (KC[C]) => Exp[C] = _ => KnStar()

  override def tyFloat(): (KC[C]) => Exp[C] = _ => KnStar()

  override def tyNat(): (KC[C]) => Exp[C] = _ => KnStar()

  override def tyString(): (KC[C]) => Exp[C] = _ => KnStar()

  override def tyUnit(): (KC[C]) => Exp[C] = _ => KnStar()

  override def tyVar(x: String): (KC[C]) => Exp[C] = c => c(x)

  override def tyRef(t: _Exp2[B, C]): (KC[C]) => Exp[C] = c =>
    apply(t)(c) match {
      case KnStar() => KnStar()
      case _ => typeError()
    }

  override def tyAbs(x: String, k: Exp[C], t: _Exp2[B, C]): (KC[C]) => Exp[C] =
    c => KnArr(k, apply(t)(c + (x -> k)))

  override def tyApp(t1: _Exp2[B, C], t2: _Exp2[B, C]): (KC[C]) => Exp[C] = c => {
    apply(t1)(c) match {
      case KnArr(k1, k2) if kEquals(k1)(apply(t2)(c)) => k2
      case _ => typeError()
    }
  }

  override def tyAll(x: String, k: Exp[C], t: _Exp2[B, C]): (KC[C]) => Exp[C] = c =>
    apply(t)(c + (x -> k)) match {
      case KnStar() => KnStar()
      case _ => typeError()
    }

  override def tySome(x: String, k: Exp[C], t: _Exp2[B, C]): (KC[C]) => Exp[C] = c =>
    apply(t)(c + (x -> k)) match {
      case KnStar() => KnStar()
      case _ => typeError()
    }

  override def tyRecord(l: List[(String, _Exp2[B, C])]): (KC[C]) => Exp[C] = c => {
    val b = l.forall(f => apply(f._2)(c) match {
      case KnStar() => true
      case _ => false
    })
    if (b) KnStar() else typeError()
  }
}

object Kinding extends Kinding[TAlg, KAlg] with TImpl[(KC[KAlg]) => Exp[KAlg]] {
  override val kEquals: (Exp[KAlg]) => (Exp[KAlg]) => Boolean = _ (KEquals)
}

trait TSubst[B[-X, Y, -Z] <: TAlg[X, Y, Z], C] extends TAlg.Transform[B, C]
  with typed.TSubst[({type l[-X, Y] = B[X, Y, C]})#l] {

  override def tyAbs(x: String, k: C, t: Exp2[B, C]): Exp2[B, C] =
    TyAbs(x, k, if (m.contains(x)) t else apply(t))

  override def tyAll(x: String, k: C, t: Exp2[B, C]): Exp2[B, C] =
    TyAll(x, k, if (m.contains(x)) t else apply(t))

  override def tySome(x: String, k: C, t: Exp2[B, C]): Exp2[B, C] =
    TySome(x, k, if (m.contains(x)) t else apply(t))
}

class TSubstImpl(mp: Map[String, _Exp2[TAlg, KAlg]]) extends TSubst[TAlg, Exp[KAlg]] with TImpl[_Exp2[TAlg, KAlg]] {
  override val m: Map[String, _Exp2[TAlg, KAlg]] = mp
}

trait TEval[B[-X, Y, -Z] <: TAlg[X, Y, Z], C] extends TAlg.Id[B, C]
  with ISubst[({type l[-X, Y] = B[X, Y, C]})#l] {

  override def tyApp(t1: Exp2[B, C], t2: Exp2[B, C]): Exp2[B, C] = {
    val v2 = apply(t2)
    apply(t1) match {
      case TyAbs(x, _, t) => apply(t(subst(x, v2)))
      case u => TyApp(u, v2)
    }
  }
}

object TEval extends TEval[TAlg, Exp[KAlg]] with TImpl[_Exp2[TAlg, KAlg]] {
  override def subst(m: Map[String, _Exp2[TAlg, KAlg]]): TAlg[_Exp2[TAlg, KAlg], _Exp2[TAlg, KAlg], Exp[KAlg]] =
    new TSubstImpl(m)
}
