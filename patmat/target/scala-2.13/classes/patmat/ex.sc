
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree
type Bit = Int

def weight(tree: CodeTree): Int = tree match {
  case Fork(l, r, c, w) => w
  case Leaf(c, w) => w
}

def chars(tree: CodeTree): List[Char] = tree match {
  case Fork(l, r, c, w) => c
  case Leaf(c, w) => List(c)
}

def makeCodeTree(left: CodeTree, right: CodeTree) =
  Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

// Part 2: Generating Huffman trees

/**
 * In this assignment, we are working with lists of characters. This function allows
 * you to easily create a character list from a given string.
 */
def string2Chars(str: String): List[Char] = str.toList

/**
 * This function computes for each unique character in the list `chars` the number of
 * times it occurs. For example, the invocation
 *
 * times(List('a', 'b', 'a'))
 *
 * should return the following (the order of the resulting list is not important):
 *
 * List(('a', 2), ('b', 1))
 *
 * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
 * character and an integer. Pairs can be constructed easily using parentheses:
 *
 * val pair: (Char, Int) = ('c', 1)
 *
 * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
 *
 * val theChar = pair._1
 * val theInt  = pair._2
 *
 * Another way to deconstruct a pair is using pattern matching:
 *
 * pair match {
 * case (theChar, theInt) =>
 * println("character is: "+ theChar)
 * println("integer is  : "+ theInt)
 * }
 */
def times(chars: List[Char]): List[(Char, Int)] = {
  def timesAcc(chars: List[Char], map: Map[Char, Int]): Map[Char, Int] = {
    if (chars.isEmpty) map
    else timesAcc(chars.tail, map + (chars.head -> (map.getOrElse(chars.head, 0) + 1)))
  }
  timesAcc(chars, Map[Char, Int]()).toList
}


/**
 * @param x  new pair
 * @param xs a sorted list of pairs
 * @return new sorted list of pairs including x
 */
def insertPair(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x._2 > y._2) y :: insertPair(x, ys)
    else x :: xs
  }
}

def insertSort(xs: List[(Char, Int)]): List[(Char, Int)] = {
  if (xs.tail.isEmpty) xs
  else insertPair(xs.head, insertSort(xs.tail))
}

/**
 * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
 *
 * The returned list should be ordered by ascending weights (i.e. the
 * head of the list should have the smallest weight), where the weight
 * of a leaf is the frequency of the character.
 */
def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
  def sortedList(sortedFreqs: List[(Char, Int)]): List[Leaf] = {
    if (sortedFreqs.tail.isEmpty) List(Leaf(sortedFreqs.head._1, sortedFreqs.head._2))
    else Leaf(sortedFreqs.head._1, sortedFreqs.head._2) :: makeOrderedLeafList(sortedFreqs.tail)
  }
  sortedList(insertSort(freqs))
}

/**
 * Checks whether the list `trees` contains only one single code tree.
 */
def singleton(trees: List[CodeTree]): Boolean = trees.tail.isEmpty

/**
 * The parameter `trees` of this function is a list of code trees ordered
 * by ascending weights.
 *
 * This function takes the first two elements of the list `trees` and combines
 * them into a single `Fork` node. This node is then added back into the
 * remaining elements of `trees` at a position such that the ordering by weights
 * is preserved.
 *
 * If `trees` is a list of less than two elements, that list should be returned
 * unchanged.
 */
def insertTree(tree: CodeTree, xs: List[CodeTree]): List[CodeTree] = xs match {
  case List() => List(tree)
  case y :: ys => {
    if (weight(tree) > weight(y)) y :: insertTree(tree, ys)
    else tree :: xs
  }
}

def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
  case List() => trees
  case x :: List() => trees
  case x :: y :: rest => insertTree(makeCodeTree(x, y), rest)
}

/**
 * This function will be called in the following way:
 *
 *   until(singleton, combine)(trees)
 *
 * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
 * the two functions defined above.
 *
 * In such an invocation, `until` should call the two functions until the list of
 * code trees contains only one single tree, and then return that singleton list.
 */
def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = {
  if (done(trees)) trees
  else until(done, merge)(merge(trees))
}

/**
 * This function creates a code tree which is optimal to encode the text `chars`.
 *
 * The parameter `chars` is an arbitrary text. This function extracts the character
 * frequencies from that text and creates a code tree based on them.
 */
def createCodeTree(chars: List[Char]): CodeTree = {
  val orderedList = makeOrderedLeafList(times(chars))
  until(singleton, combine)(orderedList).head
}

def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
  def decodeChar(root: CodeTree, cur: CodeTree, bits: List[Bit]): List[Char] = cur match {
    case Leaf(c, w) => c :: decodeChar(root, root, bits)
    case Fork(l, r, c, w) => {
      if (bits.isEmpty) List()
      else if (bits.head == 0) decodeChar(root, l, bits.tail)
      else decodeChar(root, r, bits.tail)
    }
  }
  decodeChar(tree, tree, bits)
}

def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
  def encodeChar(ch: Char, tree: CodeTree): List[Bit] = tree match {
    case Leaf(c, w) => {
      if (c == ch) List()
      else throw new Error("Leaf:" + c + "!=" + ch)
    }
    case Fork(l, r, c, w) => {
      if (chars(l).contains(ch)) 0 :: encodeChar(ch, l)
      else if (chars(r).contains(ch)) 1 :: encodeChar(ch, r)
      else throw new Error("fork:" + l + r + "!=" + ch)
    }
  }
  def encodeIter(tree: CodeTree)(text: List[Char]): List[Bit] = {
    if (text.isEmpty) List()
    else encodeChar(text.head, tree) ::: encodeIter(tree)(text.tail)
  }
  encodeIter(tree)(text)
}

val text1 = List('e','t','t','x','x','x','e','t','t','x','x','x','t','t','x','x')
val tree1 = Fork(Fork(Leaf('e', 2), Leaf('t', 6), List('e', 't'), 8), Leaf('x', 8), List('e', 't', 'x'), 16)
//val bit1 = List(0,0,0,1,1,1,1,0,0,0,1,0,1,1,1,1,0,1,1,1)

val codeTree1: CodeTree = createCodeTree(text1)
println(codeTree1)

decode(codeTree1, encode(codeTree1)(text1))


val bit2: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,
  0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
val text2 = List('h', 'u', 'f', 'f', 'm', 'a') //, 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
val codeTree2 = createCodeTree(text2)
decode(codeTree2, encode(codeTree2)(text2))
