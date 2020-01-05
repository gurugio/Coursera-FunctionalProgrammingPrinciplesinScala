import forcomp.Anagrams.{Occurrences, Sentence, Word, dictionary, wordOccurrences}
import forcomp.Anagrams._

val exSentence = List("Yes", "Man")
val exOcc = sentenceOccurrences(exSentence)
val exOccComb = combinations(exOcc)


def getSentence(occur: Occurrences): List[Word] = {
  if (occur.isEmpty) Nil
  else if (dictionaryByOccurrences.exists(_._1 == occur)) dictionaryByOccurrences(occur)
  else {
    val occurCombi = combinations(occur)
    val beforeFold: List[List[Word]] =
      for (cc <- occurCombi
           if (dictionaryByOccurrences.exists(_._1 == cc) && !getSentence(subtract(occur, cc)).isEmpty)) yield
        dictionaryByOccurrences(cc) ::: getSentence(subtract(occur, cc))
    println(occur + "->" + beforeFold)
    if (beforeFold.isEmpty) Nil
    else beforeFold.tail.foldLeft(beforeFold.head)(_ ++ _)
 }
}

def test(sentence: Sentence): List[Sentence] = {
  if (sentence.isEmpty) Nil
  else {
    val occur = sentenceOccurrences(sentence)
    val occurCombi = combinations(occur)
    for (combi <- combinations(occur)
         if (dictionaryByOccurrences.exists(_._1 == combi) && !getSentence(subtract(occur, combi)).isEmpty)) yield
      dictionaryByOccurrences(combi) ::: getSentence(subtract(occur, combi))
  }
}

test(exSentence)
