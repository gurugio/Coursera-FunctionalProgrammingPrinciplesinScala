import forcomp.Anagrams.{Occurrences, Word}

type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

val sample: Map[Int, Set[String]] = Map(1 -> Set("11"), 2 -> Set("22", "222"))


sample ++ Map(1 -> (sample.getOrElse(1, Set()) ++ Set("1111")))

Set(1,2,3) ++ Set(4,5)

def wordOccurrences(w: Word): Occurrences = {
  val occMap = w.toLowerCase groupBy ((c: Char) => c) map {
    case (k, str) => (k, str.length)
  }
  occMap.toList.sorted
}

val dictionary: List[Word] = List("aabb", "bbaa", "abc")

val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  def body(words: List[Word], map: Map[Occurrences, List[Word]]): Map[Occurrences, List[Word]] = {
    if (words.isEmpty) map
    else {
      val occ = wordOccurrences(words.head)
      body(words.tail, map ++ Map(occ -> (words.head :: map.getOrElse(occ, List()))))
    }
  }
  body(dictionary, Map())
}
println(dictionaryByOccurrences)


