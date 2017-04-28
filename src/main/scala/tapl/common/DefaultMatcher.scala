package tapl.common

trait DefaultMatcher[E] {
  var fDefault: Option[() => E] = None

  def CaseDefault(default: => E): this.type = {
    lazy val t = default
    fDefault = Some(() => t)
    this
  }
}
