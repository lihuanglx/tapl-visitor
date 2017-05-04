package play

object CaseClass {

  trait Alg1[-R, E] {
    def Lit(x: Int): E

    def Add(e1: R, e2: R): E

    def visitE(e: R): E
  }

  trait Eval1[A[-R, _]] extends Alg1[AbsExp[A], Int] {
    override def Lit(x: Int): Int = x

    override def Add(e1: AbsExp[A], e2: AbsExp[A]): Int = visitE(e1) + visitE(e2)
  }

  trait Alg2[-R, E] extends Alg1[R, E] {
    def Sub(e1: R, e2: R): E

    def If(e1: R, e2: R, e3: R): E
  }


  trait Eval2[A[-R, _]] extends Alg2[AbsExp[A], Int] with Eval1[A] {
    override def Sub(e1: AbsExp[A], e2: AbsExp[A]): Int = visitE(e1) - visitE(e2)

    override def If(e1: AbsExp[A], e2: AbsExp[A], e3: AbsExp[A]): Int = if (visitE(e1) != 0) visitE(e2) else visitE(e3)
  }


  trait AbsExp[-A[-R, _]] {
    def apply[E](alg: A[AbsExp[A], E]): E
  }


  type Exp1 = AbsExp[Alg1]

  type Exp2 = AbsExp[Alg2]

  case class CLit[A[-X, Y] <: Alg1[X, Y]](x: Int) extends AbsExp[A] {
    override def apply[E](alg: A[AbsExp[A], E]): E = alg.Lit(x)
  }

  case class CAdd[A[-X, Y] <: Alg1[X, Y]](e1: AbsExp[A], e2: AbsExp[A]) extends AbsExp[A] {
    override def apply[E](alg: A[AbsExp[A], E]): E = alg.Add(e1, e2)
  }

  case class CSub[A[-X, Y] <: Alg2[X, Y]](e1: AbsExp[A], e2: AbsExp[A]) extends AbsExp[A] {
    override def apply[E](alg: A[AbsExp[A], E]): E = alg.Sub(e1, e2)
  }

  case class CIf[A[-X, Y] <: Alg2[X, Y]](e1: AbsExp[A], e2: AbsExp[A], e3: AbsExp[A]) extends AbsExp[A] {
    override def apply[E](alg: A[AbsExp[A], E]): E = alg.If(e1, e2, e3)
  }


  trait Copy1[A[-X, Y] <: Alg1[X, Y]] extends Alg1[AbsExp[A], AbsExp[A]] {
    override def Lit(x: Int): AbsExp[A] = CLit(x)

    override def Add(e1: AbsExp[A], e2: AbsExp[A]): AbsExp[A] = CAdd(e1, e2)
  }

  trait Copy2[A[-X, Y] <: Alg2[X, Y]] extends Alg2[AbsExp[A], AbsExp[A]] with Copy1[A] {
    override def Sub(e1: AbsExp[A], e2: AbsExp[A]): AbsExp[A] = CSub(e1, e2)

    override def If(e1: AbsExp[A], e2: AbsExp[A], e3: AbsExp[A]): AbsExp[A] = CIf(e1, e2, e3)
  }


  trait Match1[A[-X, Y] <: Alg1[X, Y]] extends Alg1[AbsExp[A], Boolean] {
    override def Lit(x: Int): Boolean = true

    override def Add(e1: AbsExp[A], e2: AbsExp[A]): Boolean = (e1, e2) match {
      case (CLit(_), CLit(_)) => true
      case _ => false
    }
  }


  def main(args: Array[String]): Unit = {
    val e1: Exp1 = CLit(10)
    val e2: Exp1 = CAdd(CLit(10), CLit(20))
    val e3: Exp1 = CAdd(CLit(1), CAdd(CLit(10), CLit(20)))

    val e11: Exp2 = e1
    val e12: Exp2 = CSub(CLit(1), CLit(2))
    val e13: Exp2 = CAdd(e12, e12)

    val match1 = new Match1[Alg1] {
      override def visitE(e: AbsExp[Alg1]): Boolean = e(this)
    }

    println(e1(match1))
    println(e2(match1))
    println(e3(match1))

    val eval2 = new Eval2[Alg2] {
      override def visitE(e: AbsExp[Alg2]): Int = e(this)
    }

    println(e11(eval2))
    println(e12(eval2))
    println(e13(eval2))
  }
}
