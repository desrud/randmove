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

trait Processor {
  def process(target: Int): Set[(Int, Int)]
}

object DLAProcessor1 extends Processor {

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
          current = next
          tmpCnt += 1
          if (tmpCnt > maxIter) cont = false
          if (abs(current) > panelSize) cont = false
        }
      }
    }

    points
  }
}

object DLAProcessor2 extends Processor {

  var maxPoint = 5

  def abs(x: (Int, Int)) = {
    math.abs(x._1) + math.abs(x._2)
  }

  val maxIter = 1000
  def process(target: Int) = {
    val points: Set[(Int, Int)] = Set((0, 0))

    var n = 0
    while(n < target) {
      //choose startPoint
      var current = (maxPoint + 4, 0)

      var tmpCnt = 0
      var cont = true
      //randomly move until some max occured or crash with cluster
      while (cont) {
        val next = DLALib.randMove(current)
        if (points.contains(next)) {
          points += current
          println(n + ": " + current)

          maxPoint = math.max(maxPoint, math.max(current._1, current._2))

          n += 1
          cont = false
          println(next)
        } else {
          current = next
          tmpCnt += 1
          if (tmpCnt > maxIter) cont = false
          if (abs(current) > maxPoint + 7) cont = false
        }
      }
    }

    points
  }
}

object Executor {
  def printRes(points: Set[(Int, Int)], fileName: String) = {
    val fw = new java.io.FileWriter(fileName)
    for (point <- points) {
      fw.write(point._1 + " " + point._2 + "\n")
    }
    fw.close
  }

  var processor: Processor = DLAProcessor2
  def doAll(n: Int) = printRes(processor.process(n), "/tmp/file" + n)
}

//TODO from where should be released => (size + 3, 0)


