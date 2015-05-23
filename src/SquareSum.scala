object Problem_06 extends App {
  val list = 1 to 100 toList

  val res1 = list.foldLeft(0)((x, y) => x + y*y)
  var res2 = list.foldLeft(0)((x, y) => x + y)
  res2 = res2 * res2
  println(res2 + " - " + res1 + "\n" + (res2-res1))
}