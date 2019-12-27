def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (y: List[Any]) :: ys => flatten(y) ++ flatten(ys)
  case y :: ys => y :: flatten(ys)
}

println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
