/**
  * Created by Андрей on 21.12.2016.
  */


import java.io.{FileWriter, PrintWriter}

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import org.scalameter.{Key, Warmer, config}

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.postfixOps
import scala.runtime.ScalaRunTime._




object main {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 6,
    Key.exec.maxWarmupRuns -> 6,
    Key.exec.benchRuns -> 5,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit ={

    // (a, b) <+> (a',b') = (a' * a, a' * b + b') -- is asociative


    //println(stringOf(parallelAdding(Array(9,	7,	5,	3,	1),Array(3,	2,	1,	8,	6))))
    measur()
    //println(oneThread(Array((45,40), (30, 50), (105, 40), (90, 20))))
    //println(parallelMult(Array((45,40), (30, 50), (105, 40), (90, 20))))
    //println(parallelMult("(()))()"))
    //println(stringOf(Adding(Array(9,	7,	5,	3,	1),Array(3,	2,	1,	8,	6))))


  }

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

 def measur() : Unit = {
    Trees.threshold = 1
    var file = "measur.txt"
    var t : Array[(Double, Double)] = Array((45,40), (30, 50), (105, 40), (90, 20))
    val time2 = standardConfig measure {
      oneThread(t)
    }
    writeToFile(file, s"Measuring of ex10 [example = " + stringOf(t) + s"]:\n\nFor 1 thread: $time2 \n\n")
    do {
      Trees.threshold = Trees.threshold * 2
      val time1 = standardConfig measure {
        parallelMult(t)
      }
      appendToFile(file, "For " + Trees.threshold + " threads: \nParallel: " + time1 + "\n\n")
    } while (Trees.threshold <= 8)
  }


  def oneThread(a: Array[(Double, Double)]): (Double, Double) = {
    var N = a.length
    var (x, y, angle) = (0.0, 0.0, 0.0)
    for (i <- 0 until N) {
      angle = (angle + a(i)._1) % 360
      x = x + a(i)._2 * cos(angle)
      y = y + a(i)._2 * sin(angle)
    }
    (math.round(x * 1000) / 1000.0, math.round(y * 1000) / 1000.0)
  }
  def parallelMult(a: Array[(Double, Double)]): (Double, Double) = {
    val N = a.length
    var t = new Array[(Double, Double, Double)](N)

    var tmp = a.map({case (x, y) => (y, x, x)})

    Trees.scanAPar(tmp, (0.0, 0.0, 0.0), move, t)

    var (x, y, z) = t(N - 1)
    (math.round(x * cos(y) * 1000) / 1000.0, math.round(x * sin(y) * 1000) / 1000.0)
  }
  def move(t: (Double, Double, Double), t1: (Double, Double, Double)): (Double, Double, Double) = {
    val (a, alpha, falpha) = t
    val (b, beta,  fbeta)  = t1
    val D = math.sqrt(a * a + b * b + 2 * a * b * cos(beta + falpha - alpha))
    if (D == 0) {
      (D, alpha, (falpha + fbeta) % 360)
    } else {
      (D, (alpha + asin(b * sin(beta + falpha - alpha) / D)) % 360, (falpha + fbeta) % 360)
    }
  }

  def asin(x: Double): Double = 180 * Math.asin(x) / math.Pi
  def cos(x: Double):  Double = Math.cos(x * math.Pi / 180)
  def sin(x: Double):  Double = Math.sin(x * math.Pi / 180)
  def brackBalance(p: (Int, Int), q: (Int, Int)): (Int, Int) = (p._1 + q._1 - math.min(p._1, q._2), p._2 + q._2 - math.min(p._1, q._2))
  def pairMult(a: (Int, Int), a1: (Int, Int)) : (Int, Int) = (a1._1 * a._1, a1._1 * a._2 + a1._2)
  def f(c1 : Char, c2 : Char) : Char = if (c2 == 'M') c1 else c2
  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
