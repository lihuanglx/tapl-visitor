package tapl.language.fullomega

import tapl.common._
import tapl.component._
import tapl.language.fullomega.Term.{Query, Transform, Map2}
import tapl.language.fullomega.Term.Factory._

import scala.collection.mutable

trait Eval[A[-R, E, -T, -K] <: Term[R, E, T, K], B[-X, Y, -Z] <: Type[X, Y, Z], C[-X, Y] <: Kind[X, Y]]
  extends Term[_Exp3[A, B, C], mutable.MutableList[_Exp3[A, B, C]] => _Exp3[A, B, C], _Exp2[B, C], Exp[C]]
    with typed.Term.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with extension.Term.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with pack.Term.Lifter[_Exp3[A, B, C], _Exp3[A, B, C], _Exp2[B, C], mutable.MutableList[_Exp3[A, B, C]]]
    with ref.Eval[A[-?, ?, _Exp2[B, C], Exp[C]]] with ISubst[A[-?, ?, _Exp2[B, C], Exp[C]]] {

  override def propagate(c: mutable.MutableList[_Exp3[A, B, C]]) =
    new typed.Eval[A[-?, ?, -?, Exp[C]], _Exp2[B, C]]
      with extension.Eval[A[-?, ?, -?, Exp[C]], _Exp2[B, C]]
      with pack.Eval[A[-?, ?, -?, Exp[C]], _Exp2[B, C]] {

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

object Eval extends Eval[Term, Type, Kind]
  with Impl[mutable.MutableList[_Exp3[Term, Type, Kind]] => _Exp3[Term, Type, Kind]] {

  override val isVal: Term[_Exp3[Term, Type, Kind], Boolean, _Exp2[Type, Kind], Exp[Kind]] = IsVal

  override def subst(m: Map[String, _Exp3[Term, Type, Kind]]) = new SubstImpl(m)

  override val map2 = new Map2[Term, _Exp2[Type, Kind], Exp[Kind]]
    with Impl[(_Exp2[Type, Kind] => _Exp2[Type, Kind]) => _Exp3[Term, Type, Kind]]

  override def tSubst(m: Map[String, _Exp2[Type, Kind]]) = new TSubstImpl(m)
}

trait IsVal[A[-R, E, -T0, -K0], T, K] extends Query[Exp3[A, T, K], Boolean, T, K]
  with typed.IsVal[A[-?, ?, -?, K], T] with extension.IsVal[A[-?, ?, -?, K], T]
  with pack.IsVal[A[-?, ?, -?, K], T] with ref.IsVal[A[-?, ?, T, K]] {

  override def tmTAbs(x: String, k: K, e: Exp3[A, T, K]): Boolean = true
}

object IsVal extends IsVal[Term, _Exp2[Type, Kind], Exp[Kind]] with Impl[Boolean]

trait Subst[A[-R, E, -T0, -K0] <: Term[R, E, T0, K0], T, K] extends Transform[A, T, K]
  with typed.Subst[A[-?, ?, -?, K], T] with extension.Subst[A[-?, ?, -?, K], T] with pack.Subst[A[-?, ?, -?, K], T]

class SubstImpl(mp: Map[String, _Exp3[Term, Type, Kind]])
  extends Subst[Term, _Exp2[Type, Kind], Exp[Kind]] with Impl[_Exp3[Term, Type, Kind]] {

  override val m: Map[String, _Exp3[Term, Type, Kind]] = mp
}
