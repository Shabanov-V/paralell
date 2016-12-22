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

    //println(oneThread(t))
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
    var t = Array((0, 7), (2, 9), (12, 11), (8, 4))
    val time2 = standardConfig measure {
      oneThread(t)
    }
    writeToFile(file, s"Measuring of ex8 (example = " + stringOf(t) + s"):\n\nFor 1 thread: $time2 \n\n")
    do {
      Trees.threshold = Trees.threshold * 2
      val time1 = standardConfig measure {
        parallelMult(t)
      }
      appendToFile(file, "For " + Trees.threshold + " threads: \nParallel: " + time1 + "\n\n")
    } while (Trees.threshold <= 8)
  }


  def oneThread(a: Array[(Int, Int)]): Int = {
    var N = a.length
    var x = a(0)._2
    for (i <- 1 until N) x = a(i)._1 * x + a(i)._2
    x
  }
  def parallelMult(a: Array[(Int, Int)]): Int = {
    val N = a.length
    var t = new Array[(Int, Int)](N)



    //need to parallelize -- hard -- done
    Trees.scanAPar(a, (0, 0), pairMult, t)
    /*prefix_scan(0) = c(0)
    for (i <- 1 until N)
      prefix_scan(i) = f(prefix_scan(i - 1), c(i))*/

    t(N - 1)._2
  }

  def pairMult(a: (Int, Int), a1: (Int, Int)) : (Int, Int) = (a1._1 * a._1, a1._1 * a._2 + a1._2)
  def f(c1 : Char, c2 : Char) : Char = if (c2 == 'M') c1 else c2
  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
