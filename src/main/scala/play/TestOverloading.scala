package play

object TestOverloading {

  trait Exp[-A[-R, _]] {
    def apply[E](alg: A[Exp[A], E]): E
  }

  trait A1[-R, E] {
    def lit(x: Int): E

    def add(e1: R, e2: R): E

    def apply(e: R): E
  }

  case class Lit[A[-X, Y] <: A1[X, Y]](x: Int) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.lit(x)
  }

  case class Add[A[-X, Y] <: A1[X, Y]](e1: Exp[A], e2: Exp[A]) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.add(e1, e2)
  }

  trait Lifter1[R, C, E] extends A1[R, C => E] {
    def go(c: C): A1[R, E]

    override def add(e1: R, e2: R): C => E = go(_).add(e1, e2)

    override def lit(x: Int): C => E = go(_).lit(x)
  }

  trait A2[-R, E] {
    def vr(x: String): E
  }

  case class Var[A[-X, Y] <: A2[X, Y]](x: String) extends Exp[A] {
    override def apply[E](alg: A[Exp[A], E]): E = alg.vr(x)
  }

  trait Lifter2[R, C, E] extends A2[R, C => E] {
    def go(c: C): A2[R, E]

    override def vr(x: String): C => E = go(_).vr(x)
  }

  trait A3[-R, E] extends A1[R, E] with A2[R, E]

  trait Lifter3[R, C, E] extends A3[R, C => E] with Lifter1[R, C, E] with Lifter2[R, C, E] {
    override def go(c: C): A3[R, E]
  }

  trait E1[A[-X, Y] <: A1[X, Y]] extends A1[Exp[A], Int] {
    override def lit(x: Int): Int = x

    override def add(e1: Exp[A], e2: Exp[A]): Int = apply(e1) + apply(e2)
  }

  trait E2[A[-X, Y] <: A2[X, Y]] extends A2[Exp[A], Map[String, Int] => Int] {
    override def vr(x: String): Map[String, Int] => Int = _ (x)
  }

  /*
  trait E3[A[-X, Y] <: A3[X, Y]] extends A3[Exp[A], Map[String, Int] => Int] with E2[A] {
    def go(m: Map[String, Int]) = new E1[A] {
      // ???
      override def apply(e: Exp[A]): Int = E3.this.apply(e)(m)
    }

    override def lit(x: Int): Map[String, Int] => Int =
      m => go(m).lit(x)

    override def add(e1: Exp[A], e2: Exp[A]): Map[String, Int] => Int =
      m => go(m).add(e1, e2)
  }
  */
  trait E3[A[-X, Y] <: A3[X, Y]] extends A3[Exp[A], Map[String, Int] => Int]
    with E2[A] with Lifter1[Exp[A], Map[String, Int], Int] {

    self =>

    override def go(c: Map[String, Int]): A1[Exp[A], Int] = new E1[A] {
      override def apply(e: Exp[A]): Int = self.apply(e)(c)
    }
  }

  object E3 extends E3[A3] {
    override def apply(e: Exp[A3]): Map[String, Int] => Int = e(this)
  }

  def main(args: Array[String]): Unit = {
    val e1: Exp[A3] = Add(
      Add(Lit(1), Add(Var("x"), Var("y"))),
      Add(Var("x"), Var("y"))
    )
    println(e1(E3)(Map("x" -> 2, "y" -> 3)))
  }

}

object T2 {

  trait U {
    val b: Boolean
  }

  trait T1 {
    val b: Boolean = true

    def go(): U = new U {
      override val b: Boolean = T1.this.b
    }
  }

  trait T2 {
    val b: Boolean = false
  }

  def main(args: Array[String]): Unit = {
    val t = new T1 with T2 {
      override val b: Boolean = false
    }
    println(t.go().b)
  }
}