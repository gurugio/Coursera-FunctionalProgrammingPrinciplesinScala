def countChar(letters: List[Char]): Map[Char, Int] = {
  var map = Map[Char, Int]()
  for(letter <- letters) map += Pair(letter, map.getOrElse(letter, 0) + 1)
  map
}

println(countChar("abcdabc".toList).toList)
