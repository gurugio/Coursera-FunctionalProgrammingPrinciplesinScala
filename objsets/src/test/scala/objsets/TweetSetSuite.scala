package objsets

import org.junit.Assert.assertEquals
import org.junit._

class TweetSetSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val set6 = set5.incl(new Tweet("66", "66 body", 66))
    val set7 = set6.incl(new Tweet("77", "77 body", 21))
    val set8 = set7.incl(new Tweet("88", "88 body", 22))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  @Test def `filter: on empty set`: Unit =
    new TestSets {
      assertEquals(0, size(set1.filter(tw => tw.user == "a")))
    }

  @Test def `filter: a on set5`: Unit =
    new TestSets {
      assertEquals(1, size(set5.filter(tw => tw.user == "a")))
    }

  @Test def `filter: twenty on set5`: Unit =
    new TestSets {
      assertEquals(2, size(set5.filter(tw => tw.retweets == 20)))
    }

  @Test def `union: set4c and set4d`: Unit =
    new TestSets {
      assertEquals(4, size(set4c.union(set4d)))
    }

  @Test def `union: with empty set1`: Unit =
    new TestSets {
      assertEquals(4, size(set5.union(set1)))
    }

  @Test def `union: with empty set2`: Unit =
    new TestSets {
      assertEquals(4, size(set1.union(set5)))
    }

  @Test def `mostRetweeted`: Unit =
    new TestSets {
      assertEquals("a", set2.mostRetweeted.user)
      assertEquals(20, set2.mostRetweeted.retweets)
      assertEquals("a", set3.mostRetweeted.user)
      assertEquals(20, set3.mostRetweeted.retweets)
      assertEquals("a", set4c.mostRetweeted.user)
      assertEquals(20, set4c.mostRetweeted.retweets)
      assertEquals("a", set5.mostRetweeted.user)
      assertEquals(20, set5.mostRetweeted.retweets)
      assertEquals("66", set6.mostRetweeted.user)
      assertEquals(66, set6.mostRetweeted.retweets)
      assertEquals("66", set7.mostRetweeted.user)
      assertEquals(66, set7.mostRetweeted.retweets)
      assertEquals("66", set8.mostRetweeted.user)
      assertEquals(66, set8.mostRetweeted.retweets)
    }

  @Test def `descending: set5`: Unit =
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      /*val trends2 = set6.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "66")*/
    }


  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
