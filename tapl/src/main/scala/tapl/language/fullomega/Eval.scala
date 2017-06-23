package tapl.language.fullomega

import tapl.common._
import tapl.component._
import tapl.language.fullomega.Alg.{Query, Transform, Map2}
import tapl.language.fullomega.Alg.Factory._

import scala.collection.mutable

trait Eval[A[-R, E, -T, -K] <: Alg[R, E, T, K], B[-X, Y, -Z] <: TAlg[X, Y, Z], C[-X, Y] <: KAlg[X, Y]]
  extends Alg[_Exp3[A, B, C], mutable.MutableList[_Exp3[A, B, C]] => _Exp3[A, B, C], _Exp2[B, C], Exp[C]]
    with typed.Alg.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with extension.Alg.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with pack.Alg.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with ref.Eval[({type l[-X, Y] = A[X, Y, _Exp2[B, C], Exp[C]]})#l]
    with ISubst[({type l[-X, Y] = A[X, Y, _Exp2[B, C], Exp[C]]})#l] {

  override def go(c: mutable.MutableList[_Exp3[A, B, C]]) =
    new typed.Eval[({type l[-X, Y, -Z] = A[X, Y, Z, Exp[C]]})#l, _Exp2[B, C]]
      with extension.Eval[({type l[-X, Y, -Z] = A[X, Y, Z, Exp[C]]})#l, _Exp2[B, C]]
      with pack.Eval[({type l[-X, Y, -Z] = A[X, Y, Z, Exp[C]]})#l, _Exp2[B, C]] {

      override def apply(e: _Exp3[A, B, C]): _Exp3[A, B, C] = Eval.this.apply(e)(c)

      override def subst(m: Map[String, _Exp3[A, B, C]]): A[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], Exp[C]] =
        Eval.this.subst(m)

      override val isVal: A[_Exp3[A, B, C], Boolean, _Exp2[B, C], Exp[C]] = Eval.this.isVal
    }

  val map2: A[_Exp3[A, B, C], (_Exp2[B, C] => _Exp2[B, C]) => _Exp3[A, B, C], _Exp2[B, C], Exp[C]]

  def tSubst(m: Map[String, _Exp2[B, C]]): B[_Exp2[B, C], _Exp2[B, C], Exp[C]]

  override def tmTAbs(x: String, k: Exp[C], e: _Exp3[A, B, C]) = _ => TmTAbs(x, k, e)

  override def tmTApp(e: _Exp3[A, B, C], t: _Exp2[B, C]): (mutable.MutableList[_Exp3[A, B, C]]) => _Exp3[A, B, C] =
    c => if (e(isVal)) e match {
      case TmTAbs(x, _, b) => b(map2)(_ (tSubst(Map(x -> t))))
      case _ => typeError()
    } else TmTApp(apply(e)(c), t)
}

object Eval extends Eval[Alg, TAlg, KAlg]
  with Impl[mutable.MutableList[_Exp3[Alg, TAlg, KAlg]] => _Exp3[Alg, TAlg, KAlg]] {

  override val isVal: Alg[_Exp3[Alg, TAlg, KAlg], Boolean, _Exp2[TAlg, KAlg], Exp[KAlg]] = IsVal

  override def subst(m: Map[String, _Exp3[Alg, TAlg, KAlg]]) = new SubstImpl(m)

  override val map2 = new Map2[Alg, _Exp2[TAlg, KAlg], Exp[KAlg]]
    with Impl[(_Exp2[TAlg, KAlg] => _Exp2[TAlg, KAlg]) => _Exp3[Alg, TAlg, KAlg]]

  override def tSubst(m: Map[String, _Exp2[TAlg, KAlg]]) = new TSubstImpl(m)
}

trait IsVal[A[-R, E, -T0, -K0], T, K] extends Query[Exp3[A, T, K], Boolean, T, K]
  with typed.IsVal[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]
  with extension.IsVal[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]
  with pack.IsVal[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]
  with ref.IsVal[({type l[-X, Y] = A[X, Y, T, K]})#l] {

  override def tmTAbs(x: String, k: K, e: Exp3[A, T, K]): Boolean = true
}

object IsVal extends IsVal[Alg, _Exp2[TAlg, KAlg], Exp[KAlg]] with Impl[Boolean]

trait Subst[A[-R, E, -T0, -K0] <: Alg[R, E, T0, K0], T, K] extends Transform[A, T, K]
  with typed.Subst[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]
  with extension.Subst[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]
  with pack.Subst[({type l[-X, Y, -Z] = A[X, Y, Z, K]})#l, T]

class SubstImpl(mp: Map[String, _Exp3[Alg, TAlg, KAlg]])
  extends Subst[Alg, _Exp2[TAlg, KAlg], Exp[KAlg]] with Impl[_Exp3[Alg, TAlg, KAlg]] {

  override val m: Map[String, _Exp3[Alg, TAlg, KAlg]] = mp
}
