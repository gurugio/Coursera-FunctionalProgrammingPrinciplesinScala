def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("empty")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}


def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => List()
  case y :: ys => {
    println("y:" + y)
    reverse(ys) ++ List(y)
  }
}
println(init(List(1,2,3,4)))
println(reverse(List(1,2,3,4,5)))

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  if (n == 0) xs.tail
  else xs.head :: removeAt(n - 1, xs.tail)
}

println(removeAt(3, List(1,2,3,4,5)))

def flatten(xs: List[Any]): List[Any] = xs match {
  case List() => List()  
  case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
  case (y: Any) :: ys => y :: flatten(ys)
}

println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

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
