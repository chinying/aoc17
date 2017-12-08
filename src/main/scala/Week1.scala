import io.Source
object Day1 {
  def main(args: Array[String]) : Unit = {
    val input = Source.fromURL(getClass.getResource("1.txt")).mkString.split("\n").head
    part1(input)
    part2(input)
  }
  def part1(input: String) {
    var ans = 0
    for (i <- 1 until input.length) {
      if (input(i) == input(i-1)) ans += (input(i) - '0').toInt
    }
    if (input(0) == input(input.length-1)) ans += (input(0) - '0').toInt
    println(ans)
  }
  
  def part2(input: String) {
    var ans = 0
    val len = input.length
    for (i <- 1 until len) {
      if (input(i) == input((i + len / 2) % len)) ans += (input(i) - '0').toInt
    }
    println(ans)
  }
}

object Day2 {
  def main(args: Array[String]) {
    val input = Source.fromURL(getClass.getResource("2.txt")).mkString.split("\n")
    part1(input)
    part2(input)
  }

  def part1(lines: Array[String]) {
    var checksum = 0
    lines.foreach(line => {
      val numbers = line.trim.split("\\s+").map(_.toInt)
      val lo = numbers.reduce(_ min _)
      val hi = numbers.reduce(_ max _)
      checksum += (hi - lo)
    })
    println(checksum)
  }

  def part2(lines: Array[String]) {
    var checksum = 0
    lines.foreach(line => {
      val numbers = line.trim.split("\\s+").map(_.toInt)
      findDivisible(numbers) match {
        case Some((a, b)) =>
          checksum += a / b
      }
    })
    println(checksum)
  }

  def findDivisible(a: Array[Int]) : Option[(Int, Int)] = {
    val l = a.length
    for (i <- 0 until l) {
      for (j <- 0 until l) {
        if (i != j && Math.max(a(i), a(j)) % Math.min(a(i), a(j)) == 0) {
          return Some( (Math.max(a(i), a(j)), Math.min(a(i), a(j))) )
        }
      }
    }
    None
  }

}

object Day3 {
  def main(args: Array[String]) {
  }
}


object Day4 {
  def main(args: Array[String]) {
    val input = Source.fromURL(getClass.getResource("4.txt")).mkString.split("\n").map(line => line.trim.split("\\s+"))
    //part1(input)
    part2(input)
  }

  def part1(lines: Array[Array[String]]) {
    var ans = 0
    lines.foreach(words => {
      if (words.distinct.size == words.size) ans += 1
    })
    println(ans)
  }

  def part2(lines: Array[Array[String]]) {
    var ans = 0
    lines.foreach(words => {
      if (!words.combinations(2).toList.exists(pair => anagram(pair(0), pair(1)))) ans += 1
    })
    println(ans)
  }

  def anagram(a: String, b: String) : Boolean = {
    if (a.length != b.length) false
    var freq = Array.fill[Int](256)(0)
    a.foreach(c => freq(c) += 1)
    b.foreach(c => freq(c) -= 1)
    freq.forall(n => n == 0)
  }


}

