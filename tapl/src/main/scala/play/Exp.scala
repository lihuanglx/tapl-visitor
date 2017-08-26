package play


object Test {

  class A(val n: Int)

  class B(val m: Int, val n: Int)

  class C(val m: Int, val n: Int, val o: Int) {
    def total: Int = m + n + o
  }

  implicit def toA(n: Int): A = new A(n)

  implicit def aToB[A1, T](a: A1)(implicit f: A1 => A): B =
    new B(a.n, a.n)

  implicit def bToC[B1, T](b: B1)(implicit f: B1 => B): C =
    new C(b.m, b.n, b.m + b.n)

  val x: Int = 0

  val y: C = new A(9)

  def main(args: Array[String]): Unit = {
    println(y.total)
  }

}


trait Exp[-A[-R, _], -B[-F, _]] {
  def apply[E](alg: A[Exp[B, B], E]): E
}


object Test2 {

  trait A1[-R, E] {
    def a0(): E

    def a1(r: R, i: Int): E

    def apply(r: R): E
  }

  case class CA0[A[-X, Y] <: A1[X, Y], B[-X, Y]]() extends Exp[A, B] {
    override def apply[E](alg: A[Exp[B, B], E]): E = alg.a0()
  }

  case class CA1[A[-X, Y] <: A1[X, Y], B[-X, Y]](e: Exp1[B], i: Int) extends Exp[A, B] {
    override def apply[E](alg: A[Exp[B, B], E]): E = alg.a1(e, i)
  }

  trait A2[-R, E] extends A1[R, E] {
    def a2(): E
  }

  case class CA2[A[-X, Y] <: A2[X, Y], B[-X, Y]]() extends Exp[A, B] {
    override def apply[E](alg: A[Exp[B, B], E]): E = alg.a2()
  }

  trait A3[-R, E] extends A2[R, E] {
    def a3(x: Int): E
  }

  type Exp1[-A[-R, _]] = Exp[A, A]

  implicit def A2ToA1[B[-X, Y]](e: Option[Exp[A2, B]]): Option[Exp[A1, B]] = {
    val fn: A2[Exp[B, B], Option[Exp[A1, B]]] = new A2[Exp[B, B], Option[Exp[A1, B]]] {
      override def a0(): Option[Exp[A1, B]] = Some(CA0[A1, B]())

      override def a1(r: Exp[B, B], i: Int): Option[Exp[A1, B]] = Some(CA1[A1, B](r, i))

      override def a2(): Option[Exp[A1, B]] = None

      override def apply(r: Exp[B, B]): Option[Exp[A1, B]] = None
    }

    e match {
      case Some(x) => x(fn)
      case None => None
    }
  }

  implicit def A3ToA2[B[-X, Y]](e: Option[Exp[A3, B]]): Option[Exp[A2, B]] = {
    val fn: A3[Exp[B, B], Option[Exp[A2, B]]] = new A3[Exp[B, B], Option[Exp[A2, B]]] {
      override def a0(): Option[Exp[A2, B]] = Some(CA0[A2, B]())

      override def a1(r: Exp[B, B], i: Int): Option[Exp[A2, B]] = Some(CA1[A2, B](r, i))

      override def a2(): Option[Exp[A2, B]] = Some(CA2[A2, B]())

      override def a3(x: Int): Option[Exp[A2, B]] = None

      override def apply(r: Exp[B, B]): Option[Exp[A2, B]] = None
    }

    e match {
      case Some(x) => x(fn)
      case None => None
    }
  }

  trait Vis[A[-X, Y] <: A3[X, Y]] extends A3[Exp[A, A], Int] {
    def narrowToA3(e: Exp[A, A]): Option[Exp[A3, A]]

    override def a1(r: Exp[A, A], i: Int): Int = {
      narrowToA3(r) match {
        case Some(_) => ???
        case _ => sys.error("")
      }
      1
    }
  }

  trait Narrowing1[A[-X, Y] <: A1[X, Y]] {
    def narrowToA1[B[-R, _]](e: Exp[A, B]): Exp[A1, B]
  }

  trait Narrowing21[B[-R, _]] extends A2[Exp[B, B], Option[Exp[A1, B]]] {
    override def a0(): Option[Exp[A1, B]] = Some(CA0[A1, B]())

    override def a1(r: Exp[B, B], i: Int): Option[Exp[A1, B]] = Some(CA1[A1, B](r, i))

    override def a2(): Option[Exp[A1, B]] = None

    override def apply(r: Exp[B, B]): Option[Exp[A1, B]] = None
  }

  def main(args: Array[String]): Unit = {
    val e: Exp1[A3] = CA1[A3, A3](CA0(), 0)

    val k: Option[Exp[A3, A3]] = Some(e)

    val t: Option[Exp[A1, A3]] = (k: Option[Exp[A2, A3]]): Option[Exp[A1, A3]]
  }

}
