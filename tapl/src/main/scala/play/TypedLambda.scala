package play

object TypedLambda {

  trait Access[+Res[_]] {
    def access[C]: Res[C]
  }

  trait CList[C, +A] extends Access[({type lam[B] = CList[B, A]})#lam]

}
