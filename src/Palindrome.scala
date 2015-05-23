object Problem_04 extends App {

   println(findPalindromeMul(3))

  /* Palindrome check, uses built in reverse */
  def isPalindrome(x: String) = {
    x.reverse == x
  }

  def findPalindromeMul(digits: Int) = {
    var maxVal = 9
    var maxPal = 0
    /* get largest multiplicand, for 3 digits its 999 */
    for(n <- 1 to digits-1)  {
      maxVal += math.pow(10, n).toInt * 9
    }
    println("maxVal: " + maxVal)
    /* kind of brute force: test all possibilities of multiplying */
    for(i <- 1 to maxVal) {
      for(j <- 1 to maxVal) {
        val product = i*j
        if (isPalindrome(product.toString) && maxPal < product) {
          maxPal = product
        }
      }
    }
    maxPal
  }
}