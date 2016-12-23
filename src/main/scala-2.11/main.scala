/**
  * Created by Андрей on 21.12.2016.
  */


import java.io.{FileWriter, PrintWriter}

import org.scalameter.{Key, Warmer, config}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.runtime.ScalaRunTime._
import scala.io.Source
import scala.reflect.io.File
import scala.collection.GenSeq
import scala.reflect.macros.blackbox

class Point(val x: Double, val y: Double) {
  def distance(q: Point): Double = (x - q.x) * (x - q.x) + (q.y - y) * (q.y - y)
  def printpoint(): Unit = {
    print(s"(${main.round2(x)}, ${main.round2(y)}) ")
  }
  def equal(point: Point): Boolean = {
    (point.x == x) && (point.y == y)
  }
}


object kMeans {

  val max = 10000.0
  val min = -10000.0

  // генерирует множество точек
  def generatePoints(k: Int, num: Int): Seq[Point] = {
    val rand = scala.util.Random
    var points: Seq[Point] = Seq()
    for (i <- 0 until num) {
      val x = min + (max - min) * rand.nextDouble
      val y = min + (max - min) * rand.nextDouble
      points = points :+ new Point(x, y)
    }
    points
  }


  //выбирает k точек случайным образом для использования в качестве начальных центров кластеров
  def initializeMeans(k: Int, points: Seq[Point]): Seq[Point] = {
    val rand = scala.util.Random
    var centres: Seq[Point] = Seq()
    var set: Set[Int] = Set()
    for (i <- 0 until k) {
      var tmp = rand.nextInt(points.length)
      do {
        tmp = rand.nextInt(points.length)
      }while (set(tmp))
      set = set + tmp
      centres = centres :+ points.apply(tmp)
    }
    centres
  }

  //для точки находит ближайший из центров кластеров
  def findClosest(p: Point, means: GenSeq[Point]): Point = {
    var res = means(0)
    var m_dist = means(0).distance(p)
    for (i <- 0 until means.length) {
      val dist = means(i).distance(p)
      if (dist < m_dist) {
        m_dist = dist
        res = means(i)
      }
    }
    res
  }

  def findIndex(p: Point, means: GenSeq[Point]) : Int = {
    var tmp = -1
    for (i <- 0 to means.length) {
      if (means.apply(i) == p)
        tmp = i
    }
    tmp
  }

  //возвращает последовательность из элементов (центр кластера, <точки, для которых этот центр ближайший>)
  def classify(points: GenSeq[Point], means: GenSeq[Point]): GenSeq[(Point, GenSeq[Point])] = {
    val newMeans = points.map((p: Point) => (p, findClosest(p, means))).groupBy(_._2).toSeq
    val emptyMeans = means.diff(newMeans.map(_._1)).map((_, GenSeq()))
    newMeans.map({ case (q: Point, seq: GenSeq[(Point, Point)]) => (q, seq.map(_._1)) }) ++ emptyMeans
  }

  //вычисляет новый центр кластера; если после classify в кластере нет ни одной, возвращается старое значение
  def findAverage(oldMean: Point, points: GenSeq[Point]): Point = {
    if (points.length == 0)
      oldMean
    else {
      val sumx = points.aggregate(0.0)((z, p) => z + p.x, _ + _)
      val sumy = points.aggregate(0.0)((z, p) => z + p.y, _ + _)
      new Point(sumx / points.length, sumy / points.length)
    }
  }

  //вычисляет новые центры кластеров
  def update(classified: GenSeq[(Point, GenSeq[Point])]): GenSeq[Point] = {
    classified.map({ case (p, seq) => findAverage(p, seq) })
  }


  //определяет, сошлись ли итерации (абсолютное изменение положения для каждого меньше eta -> true)
  def converged(eta: Double)(oldMeans: GenSeq[Point], newMeans: GenSeq[Point]): Boolean = {
    val means = oldMeans.zip(newMeans)
    means.forall({ case (a, b) => a.distance(b) < eta })
  }
  //вычисляет центры кластеров алгоритмом k-means.
  @annotation.tailrec
  final def kMeans(points: GenSeq[Point], means: GenSeq[Point], eta: Double): GenSeq[Point] = {
    val newMeans = update(classify(points, means))
    if (converged(eta)(means, newMeans))
      means
    else
      kMeans(points, newMeans, eta)
  }
}


object main {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 6,
    Key.exec.maxWarmupRuns -> 6,
    Key.exec.benchRuns -> 5,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit ={
    val tmp = kMeans.generatePoints(0, 100)
    val c = kMeans.initializeMeans(7, tmp)
    val c_par = c.par
    val time1 = standardConfig measure {
      kMeans.kMeans(tmp, c, 1)
    }
    //println("!!!")
    val time2 = standardConfig measure {
      kMeans.kMeans(tmp, c_par, 1)
    }
    writeToFile("measure.txt", s"Time of usual kMean: $time1\nTime of parallel kMean: $time2\nAcceleration = ${time1.toString().dropRight(3).toDouble/time2.toString().dropRight(3).toDouble}")
    //t.foreach(t => t.printpoint())
    //println()
    //kMeans.initializeMeans(10, t).foreach(t => t.printpoint())
    //println(stringOf(kMeans.initializeMeans(3, kMeans.generatePoints(0, 10))))
  }


  def err = writeToFile("error.txt", "ERROR")

  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
    try { f(param) } finally { param.close() }

  def writeToFile(fileName:String, data:String) =
    using (new FileWriter(fileName)) {
      fileWriter => fileWriter.write(data)
    }

  def appendToFile(fileName:String, textData:String) =
    using (new FileWriter(fileName, true)){
      fileWriter => using (new PrintWriter(fileWriter)) {
        printWriter => printWriter.println(textData)
      }
    }

  def round2(x : Double) = math.round(x * 100) / 100.0
  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
