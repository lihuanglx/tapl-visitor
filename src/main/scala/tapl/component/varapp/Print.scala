package tapl.component.varapp

import tapl.common.Exp

trait Print[A[-R, _]] extends Alg[Exp[A], String] {
  def TmVar(x: String): String = x

  def TmApp(e1: String, e2: String): String = "[" + e1 + " " + e2 + "]"
}
