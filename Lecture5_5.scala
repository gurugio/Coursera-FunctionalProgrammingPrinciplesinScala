def mapFun(xs: List[Int], f: Int => Int): List[Int] =
  (xs foldRight List[Int]())((x, y) => f(x) :: y)
println(mapFun(List(1,2,3), ((x: Int) => x * x)))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x: T, y: Int) => y + 1)
println(lengthFun(List(1,2,3)))
