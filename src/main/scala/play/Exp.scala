package play

trait Visitor {
  type R
  type E

  def apply(e: R): E
}

trait Exp {
  type A <: Visitor
  type Vis[X, Y] = A {type R = X; type E = Y}

  def apply[E](alg: Vis[Exp, E]): E
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

object Test {
  /*
  type CurrVisitor = VAll {type R = CurrExp}
  type CurrExp = Exp {type A = VAll}

  val EvalAllImpl: CurrVisitor = new EvalAll {
    override type R = CurrExp

    override def apply(e: CurrExp): Int = e(EvalAllImpl)
  }
  */
}

