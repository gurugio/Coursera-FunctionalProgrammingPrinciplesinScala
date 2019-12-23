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
