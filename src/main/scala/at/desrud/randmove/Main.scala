package at.desrud.randmove

import collection.mutable.Set
import util.Random
import scala.annotation.tailrec

object Main {

  val DEFAULT_NUM_ITERATIONS = 10000
  val DEFAULT_NUM_FILES = 20

  def main(args: Array[String]): Unit = {
    val numIterations = (if (args.length > 0) tryParseInt(args(0)) else None).getOrElse(DEFAULT_NUM_ITERATIONS)
    val numFiles = (if (args.length > 1) tryParseInt(args(1)) else None).getOrElse(DEFAULT_NUM_FILES)

    println("processing " + numIterations + " iterations, generating " + numFiles + " snapshot files")
    Executor.generateAnimation(numIterations, numFiles)
  }

  def tryParseInt(x: String) = {
    try  {
      Some(x.toInt)
    } catch {
      case e: NumberFormatException => None
    }
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

  def process(numTotalPoints: Int, initial: Set[(Int, Int)]) = {
    val startTime = System.currentTimeMillis

    //random movement until crash with cluster or too far away
    @tailrec
    def randomMovement(point: (Int, Int), points: Set[(Int, Int)], maxRadius: Double): Option[(Int, Int)] = {
      val next = DLALib.randMove(point)
      if (points.contains(next)) {
        Some(point)
      } else if (abs(next) > (1.5 * maxRadius) + 5) {
        None
      } else {
        randomMovement(next, points, maxRadius)
      }
    }

    @tailrec
    def calculate(pointsLeft: Int, points: Set[(Int, Int)], maxRadius: Double): Set[(Int, Int)] = {
      if (pointsLeft == 0) {
        points
      } else if (System.currentTimeMillis - startTime > TIMEOUT) {
        println("timeout ... skipping " + pointsLeft + " points")
        points
      } else {
        //new particles will be generated at maxRadius + some offset using random phi
        val randPoint = randomPoint(maxRadius + START_OFFSET)
        val nextPoint = randomMovement(randPoint, points, maxRadius)
        nextPoint match {
          case Some(point) => calculate(pointsLeft - 1, points += point, math.max(maxRadius, abs(point)))
          case None => calculate(pointsLeft, points, maxRadius)
        }
      }
    }

    calculate(numTotalPoints, Set() ++ initial, initial.map(abs(_)).max)
  }
}

object Executor {
  var PROCESSOR: Processor = DLAProcessor
  var PLOT_SIZE = 200
  var OUTPUT_DIR = "/tmp/"

  def generateDLA(n: Int) = FileLib.printPoints(PROCESSOR.process(n), OUTPUT_DIR + "file" + n)

  def generateAnimation(numIterations: Int, numSnapshots: Int, initalSet: Set[(Int, Int)] = Set((0, 0))) {
    val iterationsPerStep = numIterations / numSnapshots
    val targetFilesNames = (1 to numSnapshots).map(OUTPUT_DIR + "file" + "%05d".format(_))

    def oneStep(set: Set[(Int, Int)], fileName: String): Set[(Int, Int)] = {
      val out = PROCESSOR.process(iterationsPerStep, set)
      FileLib.printPoints(out, fileName)
      FileLib.generateGnuplotFile(fileName, PLOT_SIZE, out.size)
      println("generated " + fileName)
      out
    }

    targetFilesNames.foldLeft(initalSet)(oneStep)
    FileLib.generateAnimationScript(OUTPUT_DIR + "animate.sh", OUTPUT_DIR + "animation.gif", targetFilesNames.toList)
  }
}
