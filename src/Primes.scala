object Primehelpers {

  def primefactors(number: Int, list: List[Int] = List()): List[Int] = {
    for (n <- 2 to number if (number % n == 0)) {
      return primefactors(number / n, list :+ n)
    }
    list
  }

  def sieve(nums: Stream[Int]): Stream[Int] = {
    Stream.cons(nums.head,
      sieve((nums.tail) filter (x => x % nums.head != 0)))
  }
  // All primes as a lazy sequence
  val primes = sieve(Stream.from(2))

  def isPrime(n: Int): Boolean =
    (2 until n) forall (i => n % i != 0)

  def isPrime2(n: Long) = {
    !(2L to math.sqrt(n).toLong).exists(n % _ == 0)
  }
}


object Problem_03 extends App {

  //println(primefactors(24))
  println(largestPrimeFactor(BigInt("600851475143")))
  val x = BigInt("333")
  val y = BigInt("222")
  println(x+y)
  //val z = BigInt(x+y)
  y.to(x, BigInt(1))
  println(x)
  println(y)

  def largestPrimeFactor(x : BigInt) = {
    def loop(factor:BigInt, number: BigInt): BigInt =
      if (factor == number) number
      else if (number % factor == 0) loop(factor, number / factor)
      else loop(factor + 1, number)
    loop (BigInt(2), x)
  }
}

object Problem_05 extends App {
  val list1 = 1 to 10 toList
  val list2 = List(1,1)
  println(list1.diff(list2))
  println(list2.diff(list1))

  val maybe = 2 * 3 * 2 * 5 * 7 * 2 * 3 * 11 * 13 * 2 * 19
  println(maybe)

  val multiples20 = multiples(20)
  println(multiples20)

  val result = multiples20.foldLeft(1)((x, y) => x*y)
  println(result)


  def multiples(number: Int): List[Int]= {
    def loop(number: Int, n: Int, primefactors: List[Int]): List[Int] = {
      if(n >= number) return primefactors
      val primefactorsTmp = Primehelpers.primefactors(n)
      //println(primefactors ::: primefactorsTmp.diff(primefactors))
      loop(number, n+1, primefactors ::: primefactorsTmp.diff(primefactors))
    }
    loop(number, 2, List())
  }
}

object Problem_07 extends App {
  println(Primehelpers.primes.take(6).toList)
  println (Timer.time {
    println(Primehelpers.primes.take(10001).last)
  })

}

object Problem_10 extends App {

  var primes = Primehelpers.primes
  var sum = 0
  println(Timer.time {
    val list  = List.range(2L,2000000L)
    val primes = list.filter(x => Primehelpers.isPrime2(x))
    println(primes)
    println(primes.sum)
  })


}
