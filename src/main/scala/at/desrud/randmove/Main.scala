package at.desrud.randmove

import collection.mutable.Set
import util.Random
import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    println("move")
  }
}

object DLALib {
  val directions = (for (i <- -1 to 1; j <- -1 to 1; if !(i == 0 && j == 0)) yield (i, j)).toArray[(Int, Int)]
  val numDirections = directions.size

  def randDir = {
    directions(Random.nextInt(numDirections))
  }

  def randMove(pnt: (Int, Int)) = {
    val diff = randDir
    (pnt._1 + diff._1, pnt._2 + diff._2)
  }
}

object FileLib {
  var ANIMATION_DELAY = 40

  def printPoints(points: Set[(Int, Int)], fileName: String) = {
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
    val filesToAnimate = files.map(_ + ".png").mkString(" ")
    fw.write("convert -delay " + ANIMATION_DELAY + " " + filesToAnimate + " " + animation + "\n")

    fw.close
  }
}

trait Processor {
  def process(target: Int): Set[(Int, Int)] = process(target, Set((0, 0)))
  def process(target: Int, initial: Set[(Int, Int)]): Set[(Int, Int)]
}

object DLAProcessor extends Processor {
  var START_OFFSET = 3
  var TIMEOUT = 120000L

  def abs(x: (Int, Int)) = {
    math.sqrt((x._1 * x._1 + x._2 * x._2).toDouble)
  }

  def randomPoint(r: Double) = {
    val phi = math.random * 2.0 * math.Pi
    val x = r * math.cos(phi)
    val y = r * math.sin(phi)
    (x.toInt, y.toInt)
  }

  def process(target: Int, initial: Set[(Int, Int)]) = {
    val startTime = System.currentTimeMillis
    val points: Set[(Int, Int)] = Set() ++ initial//TODO can get rid of this?

    //random movement until crash with cluster or too far away
    @tailrec
    def randomMovement(point: (Int, Int), maxRadius: Double): Option[(Int, Int)] = {
      val next = DLALib.randMove(point)
      if (points.contains(next)) {
        Some(point)
      } else if (abs(next) > (1.5 * maxRadius) + 5) {
        None
      } else {
        randomMovement(next, maxRadius)
      }
    }

    @tailrec
    def calculate(pointsLeft: Int, maxRadius: Double): Set[(Int, Int)] = {
      if (pointsLeft == 0) {
        points
      } else if (System.currentTimeMillis - startTime > TIMEOUT) {
        println("timeout ... skipping " + pointsLeft + " points")
        points
      } else {
        //new particles will be generated at maxRadius + some offset using random phi
        val randPoint = randomPoint(maxRadius + START_OFFSET)
        val nextPoint = randomMovement(randPoint, maxRadius)
        nextPoint match {
          case Some(point) =>
            points += point
            calculate(pointsLeft - 1, math.max(maxRadius, abs(point)))
          case None => calculate(pointsLeft, maxRadius)
        }
      }
    }

    calculate(target, initial.map(abs(_)).max)
  }
}

object Executor {
  var PROCESSOR: Processor = DLAProcessor
  var PLOT_SIZE = 200
  var FILE_PREFIX = "/tmp/file"

  def doAll(n: Int) = FileLib.printPoints(PROCESSOR.process(n), "/tmp/file" + n)

  def doAll(numIterations: Int, numSnapshots: Int, init: Set[(Int, Int)] = Set((0, 0))) {
    val stepSize = numIterations / numSnapshots
    val targetFilesNames = for (i <- 1 to numSnapshots) yield FILE_PREFIX + "%05d".format(i)

    def oneStep(set: Set[(Int, Int)], fileName: String): Set[(Int, Int)] = {
      val out = PROCESSOR.process(stepSize, set)
      FileLib.printPoints(out, fileName)
      FileLib.generateGnuplotFile(fileName, PLOT_SIZE, out.size)
      println("generated " + fileName)
      out
    }

    targetFilesNames.foldLeft(init)(oneStep)
    FileLib.generateAnimationScript("/tmp/animate", "/tmp/ani.gif", targetFilesNames.toList)
  }
}
