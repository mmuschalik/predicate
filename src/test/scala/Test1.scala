import org.junit.Test
import org.junit.Assert._

import Prolog.ADT._
import Prolog._

class Test1 {
  @Test def containsVariable(): Unit = {
    val t = predicate("p", "test", predicate("q", "test", A), B)
    assertEquals(t.contains(A), true)
    assertEquals(t.contains(B), true)
    assertEquals(t.contains(C), false)
  }
  @Test def UnifyPrredicate(): Unit = {
    val t = unify(predicate("lunch", "sandwich"), predicate("lunch", "sandwich"))
    assertEquals(t.isDefined, true)
  }

}
