package play

import scala.language.reflectiveCalls

object MultiSorts {

  trait EAlg1[-R, E] {
    def Lit(x: Int): E

    def Add(e1: R, e2: R): E

    def apply(e: R): E
  }

  trait E1[A, B, E] extends EAlg1[B, E] {
    implicit def i(e: B): A

    val alg: EAlg1[A, E]

    override def Lit(x: Int): E = alg.Lit(x)

    override def Add(e1: B, e2: B): E = alg.Add(e1, e2)

    override def apply(e: B): E = alg.apply(e)
  }

  trait EAlg11[-R, E] extends EAlg1[R, E] {
    def Add2(e1: R, e2: R): E
  }

  trait E2[A, B, E] extends EAlg11[B, E] with E1[A, B, E] {
    override val alg: EAlg11[A, E]

    override def Add2(e1: B, e2: B): E = alg.Add2(e1, e2)
  }


  case class CLit[A[-X, Y] <: EAlg1[X, Y]](x: Int) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.Lit(x)
  }

  case class CAdd[A[-X, Y] <: EAlg1[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.Add(e1, e2)
  }

  trait TAlg1[-F, T] {
    def IntT(): T

    def ArrT(t1: F, t2: F): T

    def applyT(t: F): T
  }

  trait Exp[-A[-R, _]] {
    def apply[E](alg: A[Exp[A], E]): E
  }

  trait Exp2X[-A[-R, _, -F, _], -B[-R, _]] {
    def apply[E](alg: A[Exp2X[A, B], E, Exp[B], _]): E
  }

  implicit def convert[B[-R, E, -F, T] <: EAlg1[R, E], C[-R, E]](e: Exp[EAlg1]): Exp2X[B, C] = new Exp2X[B, C] {
    override def apply[E](alg: B[Exp2X[B, C], E, Exp[C], _]): E = {
      val alg2: EAlg1[Exp2X[B, C], E] = alg
      val _alg: EAlg1[Exp[EAlg1], E] = new E1[Exp2X[B, C], Exp[EAlg1], E] {
        override implicit def i(e: Exp[EAlg1]): Exp2X[B, C] = convert(e)

        override val alg: EAlg1[Exp2X[B, C], E] = alg2
      }
      e.apply(_alg)
    }
  }


  trait Eval1[A[-R, _]] extends EAlg1[Exp[A], Int] {
    override def Lit(x: Int): Int = x

    override def Add(e1: Exp[A], e2: Exp[A]): Int = apply(e1) + apply(e2)
  }

  type Exp1 = Exp[EAlg1]

  val e1: Exp1 = CAdd(CLit(2), CLit(3))

  // -------------

  trait EAlg2[-R, E, -F, T] extends EAlg1[R, E] {
    def Lam(x: String, t: F, e: R): E
  }


  //trait Eval2[A[-R, E, -F, T], B[-F, T]] extends EAlg2[Exp2X[A, B], Int, Exp[B], Unit] with Eval1[({type lam[X, Y] = A[X, Y, Exp[B], Unit]})#lam] {
  //}

  case class CLam[A[-R, E, -F, T] <: EAlg2[R, E, F, T], B[-R, K]](x: String, t: Exp[B], e: Exp2X[A, B]) extends Exp2X[A, B] {
    override def apply[E](alg: A[Exp2X[A, B], E, Exp[B], _]): E = alg.Lam(x, t, e)
  }


  type Exp2 = Exp2X[EAlg2, TAlg1]

  val e2: Exp2 = e1

  def main(args: Array[String]): Unit = {

  }

}


object M2 {

  trait EAlg1[-R, E] {
    def Lit(x: Int): E

    def Add(e1: R, e2: R): E

    def apply(e: R): E
  }

  trait Exp[-A[-R, _]] {
    def apply[E](alg: A[Exp[A], E]): E
  }

  case class CLit[A[-X, Y] <: EAlg1[X, Y]](x: Int) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.Lit(x)
  }

  case class CAdd[A[-X, Y] <: EAlg1[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.Add(e1, e2)
  }

  trait Eval1[A[-R, _]] extends EAlg1[Exp[A], Int] {
    override def Lit(x: Int): Int = x

    override def Add(e1: Exp[A], e2: Exp[A]): Int = apply(e1) + apply(e2)
  }


  trait TAlg1[-F, T] {
    def IntT: T

    def IdT(t: F): T

    def apply(t: F): T
  }

  case class CIntT[A[-X, Y] <: TAlg1[X, Y]]() extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.IntT
  }

  case class CIdT[A[-X, Y] <: TAlg1[X, Y]](t: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.IdT(t)
  }

  trait TAlg2[-F, T] extends TAlg1[F, T] {
    def ArrT(t1: F, t2: F): T
  }

  case class CArrT[A[-X, Y] <: TAlg2[X, Y]](t1: Exp[A], t2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.ArrT(t1, t2)
  }

  trait EAlg2[-R, E, -F] extends EAlg1[R, E] {
    def Anno(e: R, t: F): E
  }

  type E3[-A[-R, E, -F], +V] = Exp[({type lam[-X, Y] = A[X, Y, V]})#lam]

  case class CAnno[A[-R, E, -F] <: EAlg2[R, E, F], V](e: E3[A, V], t: V) extends E3[A, V] {
    override def apply[E](alg: A[Exp[({type lam[-X, Y] = A[X, Y, V]})#lam], E, V]): E = alg.Anno(e, t)
  }

  trait EVal2[A[-R, E, -F], V] extends EAlg2[E3[A, V], Int, V] with Eval1[({type lam[-X, Y] = A[X, Y, V]})#lam] {
    override def Anno(e: E3[A, V], t: V): Int = apply(e)
  }

  //------

  type Exp1 = Exp[EAlg1]

  val e1: Exp1 = CAdd(CLit(2), CLit(3))


  type Aux2[-R, E] = EAlg2[R, E, Exp[TAlg1]]

  type Exp2 = Exp[Aux2]

  val e2: Exp2 = e1
  val e22: Exp2 = CAnno[EAlg2, Exp[TAlg1]](e2, CIntT())
  val e23: Exp2 = CAdd(CLit(2), e22)


  type Aux3[-R, E] = EAlg2[R, E, Exp[TAlg2]]

  //type Exp3 = E3[EAlg2, Exp[TAlg2]]
  type Exp3 = Exp[Aux3]

  val e3: Exp3 = e1
  val e33: Exp3 = e2
  val e34: Exp3 = e23
  val e35: Exp3 = e22
  val e36: Exp3 = CAnno[EAlg2, Exp[TAlg2]](e23, CArrT(CIdT(CIntT()), CIntT()))


  def main(args: Array[String]): Unit = {
    val eval2 = new EVal2[EAlg2, Exp[TAlg1]] {
      override def apply(e: Exp[({type lam[-X, Y] = EAlg2[X, Y, Exp[TAlg1]]})#lam]): Int = e(this)
    }
    println(e2(eval2))
    println(e22(eval2))
    println(e23(eval2))

    val eval3 = new EVal2[EAlg2, Exp[TAlg2]] {
      override def apply(e: Exp[({type lam[-X, Y] = EAlg2[X, Y, Exp[TAlg2]]})#lam]): Int = e(this)
    }
    println(e3(eval3))

    testMatch(e36)
  }


  def testMatch(e: Exp3): Unit = e match {
    case CAnno(_, _) => println("Anno")
    case CAdd(_, _) => println("Add")
    case _ => println("Other")
  }

}
