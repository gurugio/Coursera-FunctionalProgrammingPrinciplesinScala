val data = List('a','a','a','b','b','c','a','a')

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => 
    val (first, rest) = xs.span(y => y == x)
    first :: pack(rest)
}

def encode(xs: List[Char]): List[(Char, Int)] = {
  val packed = pack(xs)
  def encodePacked(plist: List[List[Char]]): List[(Char, Int)] = plist match {
    case Nil => Nil
    case first :: rest => (first.head, first.length) :: encodePacked(rest)
  }
  encodePacked(packed)
}

println(encode(data))
