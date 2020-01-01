package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(5, weight(t1))
    }


  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `string2chars hello world`: Unit =
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times test`: Unit =
    assertEquals(List(('a',3)), times("aaa".toList))

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `singleton test`: Unit =
    new TestTrees {
      assertEquals(true, singleton(List(Leaf('a',2))))
      assertEquals(false, singleton(List(Leaf('a',2), Leaf('b', 3))))
    }

  @Test def `combine of some leaf list (15pts)`: Unit = {
    new TestTrees {
      val leaflist1 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      assertEquals(List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)), combine(leaflist1))
      val leaflist2 = List(Leaf('e', 10), Leaf('t', 20), Leaf('x', 22))
      assertEquals(List(Leaf('x', 22), Fork(Leaf('e', 10), Leaf('t', 20), List('e', 't'), 30)), combine(leaflist2))
      val leaflist3 = List(Leaf('e', 2), Leaf('t', 6), Leaf('x', 8)) // ettxxxettxxxttxx
      assertEquals(List(Fork(Leaf('e', 2), Leaf('t', 6), List('e', 't'), 8), Leaf('x', 8)), combine(leaflist3))
    }
  }

  @Test def `merge`: Unit = {
    new TestTrees {
      val leaflist1 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
      assertEquals(List(Fork(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7)),
        until(singleton, combine)(leaflist1))
      val leaflist2 = List(Leaf('e', 10), Leaf('t', 20), Leaf('x', 22))
      assertEquals(List(Fork(Leaf('x',22),Fork(Leaf('e',10),Leaf('t',20),List('e', 't'),30),List('x', 'e', 't'),52)),
        until(singleton, combine)(leaflist2))
      val leaflist3 = List(Leaf('e', 2), Leaf('t', 6), Leaf('x', 8)) // ettxxxettxxxttxx
      assertEquals(List(Fork(Fork(Leaf('e', 2), Leaf('t', 6), List('e', 't'), 8), Leaf('x', 8), List('e','t','x'),16)),
        until(singleton, combine)(leaflist3))
    }
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit = {
    new TestTrees {
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
