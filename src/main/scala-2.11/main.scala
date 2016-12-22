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
    //println(oneThread("(())("))
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
    var t = "(())()"
    val time2 = standardConfig measure {
      oneThread(t)
    }
    writeToFile(file, s"Measuring of ex9 [example = " + stringOf(t) + s"]:\n\nFor 1 thread: $time2 \n\n")
    do {
      Trees.threshold = Trees.threshold * 2
      val time1 = standardConfig measure {
        parallelMult(t)
      }
      appendToFile(file, "For " + Trees.threshold + " threads: \nParallel: " + time1 + "\n\n")
    } while (Trees.threshold <= 8)
  }


  def oneThread(a: String): Boolean = {
    var N = a.length
    var x = 0
    var f = true
    for (i <- 0 until N) {
      if (x < 0)
        f = false
      if (a(i) == '(')
        x += 1
      else
        x -= 1
    }
    x == 0 && f
  }
  def parallelMult(a: String): Boolean = {
    val N = a.length
    var t = new Array[(Int, Int)](N)


    Trees.scanAPar(a.map({case '(' => (1, 0) ;case ')' => (0, 1)}).toArray, (0, 0), brackBalance, t)

    t(N - 1) == (0, 0)
  }

  def brackBalance(p: (Int, Int), q: (Int, Int)): (Int, Int) = (p._1 + q._1 - math.min(p._1, q._2), p._2 + q._2 - math.min(p._1, q._2))
  def pairMult(a: (Int, Int), a1: (Int, Int)) : (Int, Int) = (a1._1 * a._1, a1._1 * a._2 + a1._2)
  def f(c1 : Char, c2 : Char) : Char = if (c2 == 'M') c1 else c2
  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
