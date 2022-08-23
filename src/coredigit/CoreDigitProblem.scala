package coredigit

import scala.annotation.tailrec

object CoreDigitProblem extends App {

  @tailrec
  def coreDigit(n: Long,mul : Int =1): Long = {
    if(n<10) n
        else coreDigit(n.toString.split("").map(c => c.toInt).sum)
  }

  def concatenateNumber(numStr: String): Long = {
    val arr: Array[Long] = numStr.split(" ").map(_.trim).map(_.toLong)
    @tailrec
    def concatenateStr(str: String, times: Long, acc: String): String = {
      if(times <= 0 ) acc
      else concatenateStr(str, times-1, acc+str)
    }
    concatenateStr(arr(0).toString, arr(1), "").toLong
  }

  println("coreDigit(9785): " + coreDigit(9785))
  println("concatenateNumber(\"9785 4\"): " + concatenateNumber("9785 4"))
  println("coreDigit(concatenateNumber(\"9785 4\")): " + coreDigit(concatenateNumber("9785 4")))


}
