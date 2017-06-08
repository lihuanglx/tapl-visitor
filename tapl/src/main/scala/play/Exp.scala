package play

import scala.language.reflectiveCalls

trait Visitor {
  type R
  type E

  def apply(e: R): E
}

trait Exp {
  self =>

  type A <: Visitor
  type Vis[X, Y] = A {type R = X; type E = Y}

  def apply[E](alg: Vis[Exp {type A = self.A}, E]): E
}

trait VA extends Visitor {
  def Lit(x: Int): E

  def If(e1: R, e2: R, e3: R): E
}

trait EvalA extends VA {
  override type E = Int

  override def Lit(x: Int): Int = x

  override def If(e1: R, e2: R, e3: R): Int = if (apply(e1) != 0) apply(e2) else apply(e3)
}

trait VB extends Visitor {
  def Add(e1: R, e2: R): E
}

trait EvalB extends VB {
  override type E = Int

  override def Add(e1: R, e2: R): Int = apply(e1) + apply(e2)
}

trait VAll extends VA with VB

trait EvalAll extends VAll with EvalA with EvalB

object EvalAllImpl extends EvalAll {
  override type R = Exp {type A = VAll}

  override def apply(e: R): Int = e(this)
}

/*
object Test {
  type CurrExp = Exp {type A = VAll}

  def Lit(x: Int): Exp = new Exp {
    type A = VA

    //override def apply[E](alg: Vis[Exp {type A = this.A}, E]): E = ???
    override def apply[E](alg: Vis[Exp, E]): E = alg.Lit(x)
  }

  def Add(e1: Exp, e2: Exp) = new Exp {
    type A = VAll

    override def apply[E](alg: Vis[Exp, E]): E = alg.Add(e1, e2)
  }

  def main(args: Array[String]): Unit = {
    val e = Add(Lit(3), Lit(4))
    println(e(EvalAllImpl))
  }
}
*/
