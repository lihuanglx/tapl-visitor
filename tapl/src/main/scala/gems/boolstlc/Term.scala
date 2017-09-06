package gems.boolstlc

import gems.common._
import gems.Lang
import gems.{bool, stlc}

@Lang("boolstlc")
trait Term[-R, E, -T] extends bool.Term[R, E] with stlc.Term[R, E, T]

@Lang("boolstlc")
trait Type[-R, E] extends bool.Type[R, E] with stlc.Type[R, E]
