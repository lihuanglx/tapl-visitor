package tapl.common

object Util {
  // todo: use monad
  def typeError(msg: String = "Type error!"): Nothing = sys.error(msg)
}
