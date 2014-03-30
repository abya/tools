package test

import scala.util.Random

object LuhnUtils {
  def main(args: Array[String]) {
    val num = generateValidLuhnSequence(16)
    println(num + ",  " + luhnTest(num))
  }

  def generateValidLuhnSequence(seqLen: Int): String = {
    val odds = List.fill(seqLen/2)(Random.nextInt(8) + 1) // from 1 to 9
    val evens = List.fill(seqLen/2)(Random.nextInt(8) + 1)
    val diff = (evens.map(sumdigits).sum + odds.sum) % 10
    val newodds = applydiff(odds, diff)
    evens.zip(newodds).flatMap(x => List(x._1, x._2)).mkString("")
  }

  def sumdigits(num: Int): Int = (2 * num).toString.toList.foldLeft(0)(_ + _ - '0')
  
  def applydiff(lst: List[Int], diff: Int): List[Int] = 
    if ((lst.head - diff) >= 1) lst.updated(0, lst.head - diff)
    else 1 :: applydiff(lst.tail, diff - lst.head + 1)

  def luhnTest(num: String): Boolean = {
    val (evens, odds) = num.zipWithIndex.partition(_._2 % 2 == 0)
    val evensum = evens.map(_._1.asDigit).map(x => sumdigits(x)).sum
    val oddsum = odds.map(_._1.asDigit).sum
    ((oddsum + evensum) % 10) == 0
  }
}
