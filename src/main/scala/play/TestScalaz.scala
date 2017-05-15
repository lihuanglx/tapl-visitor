package play

import scalaz._
import Scalaz._

object TestScalaz {

  trait T[M[_]] {
    implicit val m: MonadError[M, String]
    implicit val m2: MonadState[M, Map[String, Boolean]]

    def f(): M[Int] = m.raiseError("error")
    def g(): M[Int] = m.point(1)
  }

  def main(args: Array[String]): Unit = {
    type E[A] = \/[String, A]
    type M[A] = StateT[E, Map[String, Boolean], A]

    trait ME extends MonadError[M, String] {
      implicit val t: MonadError[E, String] = implicitly[MonadError[E, String]]
    }

    val t = new T[M] {
      override implicit val m: MonadError[M, String] = ???
      override implicit val m2: MonadState[M, Map[String, Boolean]] = StateT.stateTMonadState
    }

    println(t.f())
  }
}
