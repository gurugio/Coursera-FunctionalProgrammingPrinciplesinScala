def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x > y) y :: insert(x, ys)
    else x :: xs
  }
}

println(insert(5, List(2,4,9)))
println(isort(List(3,7,4,2,5,9)))

def insertPair(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
  case List() => List(x)
  case y :: ys => {
    if (x._2 > y._2) y :: insertPair(x, ys)
    else x :: xs
  }
}

println(insert(('a',3), List(('b', 1), ('c', 2))))

def pairSort(xs: List[(Char, Int)]): List[(Char, Int)] = xs mat {
  case y :: ys => insert(y, ys) :: pairSort(ys)
  case 
  
}
