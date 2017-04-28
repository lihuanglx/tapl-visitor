package play

trait QueryRoot[A[-X, _], R, E] extends A[R, E] {
  val default: E
}

trait A1[-R, E] {
  def TmVar(x: String): E

  def TmApp(e1: R, e2: R): E

  def apply(e: R): E
}

trait Q1[R, E] extends QueryRoot[A1, R, E] {
  override def TmVar(x: String): E = default
}