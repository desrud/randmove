package at.desrud.randmove

import collection.mutable.Set

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