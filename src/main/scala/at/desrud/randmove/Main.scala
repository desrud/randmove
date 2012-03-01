package at.desrud.randmove

import collection.mutable.Set
import util.Random

object Main {

  def main(args: Array[String]): Unit = {
    println("move")
  }
}

object DLALib {
  val directions = List((1, 0), (0, 1), (1, 1), (1, -1), (-1, 0), (-1, 1), (-1, -1), (0, -1)).toArray[(Int, Int)]
  val size = directions.size
  def randDir = {
    directions(Random.nextInt(size))
  }

  def randMove(p: (Int, Int)) = {
    val d = randDir
    (p._1 + d._1, p._2 + d._2)
  }
}

object DLA {

  val panelSize = 200
  def randStartPoint = {
    val x = Random.nextInt(2 * panelSize + 1) - panelSize
    val y = Random.nextInt(2 * panelSize + 1) - panelSize
    (x, y)
  }

  def abs(x: (Int, Int)) = {
    math.abs(x._1) + math.abs(x._2)
  }

  val maxIter = 1000
  def process(target: Int) = {
    val points: Set[(Int, Int)] = Set((0, 0))

    var n = 0
    while(n < target) {
      //choose startPoint
      var current = (0, 0)
      while (points.contains(current)) current = randStartPoint

      var tmpCnt = 0
      var cont = true
      //randomly move until some max occured or crash with cluster
      while (cont) {
        val next = DLALib.randMove(current)
        if (points.contains(next)) {
          points += current
          println(n + ": " + current)
          n += 1
          cont = false
          println(next)
        } else {
          tmpCnt += 1
          if (tmpCnt > maxIter) cont = false
          if (abs(current) > panelSize) cont = false
        }
      }
    }

    points
  }

  def printRes(points: Set[(Int, Int)], fileName: String) = {
    val fw = new java.io.FileWriter(fileName)
    for (point <- points) {
      fw.write(point._1 + " " + point._2 + "\n")
    }
    fw.close
  }

  def doAll(n: Int) = printRes(process(n), "/tmp/file" + n)
}


