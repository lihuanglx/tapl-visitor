package language
package stlc

import gems._
import Term.Factory._

trait Eval[A[-R, E, -F] <: Term[R, E, F], T] extends Term[Exp2[A, T], Exp2[A, T], T]
  with IIsVal[A[-?, ?, T]] with ISubst[A[-?, ?, T]] with Term.Convert[A, T] {

  def tmVar(x: String): Exp2[A, T] = runtimeError

  def tmAbs(x: String, t: T, e: Exp2[A, T]): Exp2[A, T] =
    TmAbs[A, A[-?, ?, T], T](x, t, e)

  def tmApp(e1: Exp2[A, T], e2: Exp2[A, T]): Exp2[A, T] =
    if (e1(isVal)) {
      if (e2(isVal)) {
        val c = convertStlc[A[-?, ?, T]](e1).getOrElse(cnvFailed)
        c(new Term.Query[Exp2[A, T], Exp2[A, T], T] {
          def default: Exp2[A, T] = runtimeError

          override def tmAbs(x: String, t: T, e: Exp2[A, T]): Exp2[A, T] =
            e(subst(x, e2))

          override def apply(e: Exp2[A, T]): Exp2[A, T] = impossible
        })
      }
      else
        TmApp[A, A[-?, ?, T], T](e1, apply(e2))
    } else
      TmApp[A, A[-?, ?, T], T](apply(e1), e2)

  def tmUnit(): Exp2[A, T] = TmUnit[A, A[-?, ?, T], T]()
}

trait IsVal[A[-R, E, -F], V] extends Term.Query[Exp2[A, V], Boolean, V] {
  override def default: Boolean = false

  override def tmUnit(): Boolean = true

  override def tmAbs(x: String, t: V, e: Exp2[A, V]) = true
}

trait Subst[A[-R, E, -F] <: Term[R, E, F], T] extends Term.Transform[A, T] {
  val u: String
  val v: Exp2[A, T]

  override def tmVar(x: String): Exp2[A, T] =
    if (u == x) v else TmVar[A, A[-?, ?, T], T](x)

  override def tmAbs(x: String, t: T, e: Exp2[A, T]): Exp2[A, T] =
    TmAbs[A, A[-?, ?, T], T](x, t, if (u == x) e else apply(e))
}

trait Print[A[-R, E, -F], T] extends Term[Exp2[A, T], String, T] {
  def printT(v: T): String

  def tmVar(x: String): String = x

  def tmAbs(x: String, t: T, e: Exp2[A, T]): String =
    s"\\$x: ${printT(t)}. ${apply(e)}"

  def tmApp(e1: Exp2[A, T], e2: Exp2[A, T]): String =
    s"(${apply(e1)}) ${apply(e2)}"

  def tmUnit(): String = "unit"
}

trait PrintT[A[-R, _]] extends Type[Exp[A], String] {
  def tyUnit(): String = "Unit"

  def tyArr(t1: Exp[A], t2: Exp[A]): String = s"(${apply(t1)})->${apply(t2)}"
}
