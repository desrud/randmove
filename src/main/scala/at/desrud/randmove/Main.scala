package at.desrud.randmove

import collection.mutable.Set
import util.Random

object Main {

  def main(args: Array[String]): Unit = {
    println("move")
  }
}

object DLALib {
  var animationDelay = 40

  val directions = (for (i <- -1 to 1; j <- -1 to 1; if !(i == 0 && j == 0)) yield (i, j)).toArray[(Int, Int)]
  val size = directions.size
  def randDir = {
    directions(Random.nextInt(size))
  }

  def randMove(pnt: (Int, Int)) = {
    val diff = randDir
    (pnt._1 + diff._1, pnt._2 + diff._2)
  }

  def printRes(points: Set[(Int, Int)], fileName: String) = {
    val fw = new java.io.FileWriter(fileName)
    for (point <- points) {
      fw.write(point._1 + " " + point._2 + "\n")
    }
    fw.close
  }

  def generateGnuplotFile(fileName: String, size: Int, numPoints: Int) =   {
    val plotRangeX = "[" + (-size) + ":" + size + "]"
    val plotRangeY = plotRangeX
    val title = "title \"n = " + numPoints + "\""
    val fileSize = 2 * size + 1

    val gpFile = fileName + ".gp"
    val fw = new java.io.FileWriter(gpFile)
    fw.write("set terminal png size " + fileSize + "," + fileSize + "\n")
    fw.write("set output \"" + fileName + ".png" + "\"\n")
    fw.write("plot " + plotRangeX + plotRangeY + " \"" + fileName + "\" " + title + " with dots\n")
    fw.close
  }

  def generateAnimationScript(fileName: String, animation: String, files:List[String]) {
    val fw = new java.io.FileWriter(fileName)

    files.foreach(x => fw.write("gnuplot " + x + ".gp\n"))
    fw.write("convert -delay " + animationDelay + " *.png " + animation)//TODO *.png might not be the best choice

    fw.close
  }
}

trait Processor {
  def process(target: Int): Set[(Int, Int)] = process(target, Set((0, 0)))
  def process(target: Int, initial: Set[(Int, Int)]): Set[(Int, Int)]
}

object DLAProcessor extends Processor {

  def abs(x: (Int, Int)) = {
    math.sqrt((x._1 * x._1 + x._2 * x._2).toDouble)
  }

  def randomPoint(r: Double) = {
    val phi = math.random * 2.0 * math.Pi
    val x = r * math.cos(phi)
    val y = r * math.sin(phi)
    (x.toInt, y.toInt)
  }

  var START_OFFSET = 3
  var TIMEOUT = 120000L

  def process(target: Int, initial: Set[(Int, Int)]) = {
    var maxRadius = 0.0
    val startTime = System.currentTimeMillis

    val points: Set[(Int, Int)] = Set() ++ initial

    var n = 0
    while(n < target) {//TODO bad

      if (System.currentTimeMillis - startTime > TIMEOUT) {
        println("timeout ... at n = " + n)
        n = target//TODO very bad
      }

      //new particles will be generated at maxRadius + 1 using random phi
      var current = randomPoint(maxRadius + START_OFFSET)

      var cont = true
      //randomly move until some max occured or crash with cluster
      while (cont) {
        val next = DLALib.randMove(current)
        if (points.contains(next)) {
          points += current

          maxRadius = math.max(maxRadius, abs(current))

          n += 1
          cont = false
          println(next)
        } else {
          current = next
          if (abs(current) > (1.5 * maxRadius) + 3) cont = false
        }
      }
    }

    points
  }
}

object Executor {
  var processor: Processor = DLAProcessor
  var plotSize = 200

  val set = Set((0, 0))
  def doAll(n: Int) = DLALib.printRes(processor.process(n), "/tmp/file" + n)

  def doAll(numIterations: Int, numSnapshots: Int, init: Set[(Int, Int)] = Set((0, 0))) {
    val step = numIterations / numSnapshots
    var set = init

    var allFiles: List[String] = Nil

    for (i <- 1 to numSnapshots) {
      set = processor.process(step, set)//TODO some points missing?
      val fileName = "/tmp/file" + "%05d".format(i)
      DLALib.printRes(set, fileName)
      DLALib.generateGnuplotFile(fileName, plotSize, set.size)
      allFiles ::= fileName
    }

    DLALib.generateAnimationScript("/tmp/animate", "/tmp/ani.gif", allFiles.reverse)
  }
}

//TODO initial box => need other startpoint algorithm and other break condition