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


def merge(xs: List[Int], ys: List[Int]): List[Int] = xs match {
  case List() => List()
  case x :: xss => ys match {
    case List() => xs
    case y :: yss => {
      if (x > y) y :: merge(xs, yss)
      else x :: merge(xss, ys)
    }
  }
}

def merge2(xs: List[Int], ys: List[Int]): List[Int] = {
  (xs, ys) match {
    case (Nil, yss) => yss
    case (xss, nil) => xss
    case (x :: xss, y :: yss) => {
      if (x > y) y :: merge2(xs, yss)
      else x :: merge2(xss, ys)
    }
  }
}

def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

println(merge2(List(2,4,6), List(1,3,5)))
println(msort(List(6,4,2,5,3,1)))

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
