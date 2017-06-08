package play.testpackage.client

import play.testpackage.pkg1.F
import play.testpackage.pkg2.T2._

object F2 {

  val v2: Int = F.v match {
    case AA(x) => x
    case _ => sys.error("error")
  }

  def main(args: Array[String]): Unit = {
    println(v2)
  }

}