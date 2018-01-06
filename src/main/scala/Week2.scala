import collection.mutable.ListBuffer
import io.Source

object Day12 {
  class UnionFind(val initSize : Int) {
    private var parent = new ListBuffer[Int]()
    private var rank = new ListBuffer[Int]()
    var count : Int = initSize // no. of components

    for (i<-0 until initSize) {
      parent += i
      rank += 0
    }

    def find(x : Int) : Int = {
      var p = x
      while (p != parent(p)) {
        parent(p) = parent(parent(p))
        p = parent(p)
      }
      p
    }

    def connected(x: Int, y: Int) = find(x) == find(y)

    def union(x: Int, y: Int) {
      var p = find(x)
      var q = find(y)
      if (p == q) return
      
		 // make root of smaller rank point to root of larger rank
			if (rank(p) < rank(q)) parent(p) = q
			else if (rank(p) > rank(q)) parent(q) = p
			else {
        parent(q) = p
        rank(p) += 1
			}
			count -= 1	
    }

    def groupSize(x: Int) : Int = {
      if (x > initSize) -1
      (0 until initSize).filter(connected(_, x) == true).length
    }

  }

  def solve(lines: Array[String]) {
    val graphSize = lines.length
    println(graphSize)

    var uf = new UnionFind(graphSize)

    lines.foreach(line => {
      val tokens = line.split("<->").map(_.trim)
      val root = tokens.head.toInt
      val neighbours = tokens.last.split(",").map(_.trim).map(_.toInt)
      // println(neighbours.mkString)
      neighbours.foreach(neighbour => {
        uf.union(root, neighbour)
      })
    })
    println(uf.groupSize(0)) // part 1
    println(uf.count) // part 2

  }

  def main(args: Array[String]) {
    val input = Source.fromURL(getClass.getResource("12.txt")).mkString.split("\n")
    solve(input)
  }
}
