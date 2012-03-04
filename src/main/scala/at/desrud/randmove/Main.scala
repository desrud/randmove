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

  def printRes(points: Set[(Int, Int)], fileName: String) = {
    val fw = new java.io.FileWriter(fileName)
    for (point <- points) {
      fw.write(point._1 + " " + point._2 + "\n")
    }
    fw.close
  }

  def generateGnuplotFile(fileName: String) =   {
    val gpFile = fileName + ".gp"
    val fw = new java.io.FileWriter(gpFile)
    fw.write("set terminal png\n")
    fw.write("set output \"" + fileName + ".png" + "\"\n")
    fw.write("plot [-100:100][-100:100] \"" + fileName + "\"\n")
    fw.close
  }

  def generateAnimationScript(fileName: String, animation: String, files:List[String]) {
    val fw = new java.io.FileWriter(fileName)

    files.foreach(x => fw.write("gnuplot " + x + ".gp\n"))
    fw.write("convert -delay 20 *.png " + animation)

    fw.close
  }
}

trait Processor {
  def process(target: Int): Set[(Int, Int)] = process(target, Set((0, 0)))
  def process(target: Int, initial: Set[(Int, Int)]): Set[(Int, Int)]
}

object DLAProcessor3 extends Processor {

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
    while(n < target) {

      if (System.currentTimeMillis - startTime > TIMEOUT) {
        println("timeout ... at n = " + n)
        n = target
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
  var processor: Processor = DLAProcessor3

  val set = Set((0, 0))
  def doAll(n: Int) = DLALib.printRes(processor.process(n), "/tmp/file" + n)

  def doAll(n: Int, numSnapshots: Int, init: Set[(Int, Int)] = Set((0, 0))) {
    val step = n / numSnapshots
    var set = init

    var allFiles: List[String] = Nil

    for (i <- 1 to numSnapshots) {
      val iter = i * step
      set = processor.process(step, set)//TODO some points missing?
      val fileName = "/tmp/file" + "%05d".format(i)
      DLALib.printRes(set, fileName)
      DLALib.generateGnuplotFile(fileName)
      allFiles ::= fileName
    }

    DLALib.generateAnimationScript("/tmp/animate", "/tmp/ani.gif", allFiles.reverse)
  }
}

//TODO initial box => need other startpoint algorithm and other break condition