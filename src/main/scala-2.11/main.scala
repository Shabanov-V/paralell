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


    //println(stringOf(parallelAdding(Array(9,	7,	5,	3,	1),Array(3,	2,	1,	8,	6))))
    measur()
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
    val time2 = standardConfig measure {
      Adding(Array(9,	7,	5,	3,	1),Array(3,	2,	1,	8,	6))
    }
    writeToFile(file, s"Measuring of Adding (example = Array(9, 7, 5, 3, 1) + Array(3, 2, 1, 8, 6) ):\n\nFor 1 thread: $time2 \n\n")
    do {
      Trees.threshold = Trees.threshold * 2
      val time1 = standardConfig measure {
        parallelAdding(Array(9,	7,	5,	3,	1),Array(3,	2,	1,	8,	6))
      }
      appendToFile(file, "For " + Trees.threshold + " threads: \nParallel: " + time1 + "\n\n")
    } while (Trees.threshold <= 8)
  }

  def Adding(a: Array[Int], b: Array[Int]): Array[Int] = {
    val N = a.length
    var t = new Array[Int](N)

    var c = new Array[Char](N)
    for (j <- 0 to N - 1)
      if (a(j) + b(j) > 9)  c(j) = 'C' else if (a(j) + b(j) == 9) c(j) = 'M' else c(j) = 'N'


    var prefix_scan = new Array[Char](N + 1)
    prefix_scan(0) = c(0)
    for (i <- 1 until N)
      prefix_scan(i) = f(prefix_scan(i - 1), c(i))

    var carries = new Array[Int](N)
    carries(0) = 0
    for (j <- 0 to N - 1)
      if (j != 0) carries(j) = bool2int(prefix_scan(j - 1) == 'C')


    var sum = new Array[Int](N)
    for (j <- 0 to N - 1)
      sum(j) = (a(j) + b(j) + carries(j)) % 10

    sum
  }
  def parallelAdding(a: Array[Int], b: Array[Int]): Array[Int] = {
    val plus = (x: Int, y: Int) => x + y
    /*val partime = standardConfig measure {
      Trees.scanAPar(a, 0, plus, t)
    }
    println(partime)*/
    //scan_leftA(a, 0, plus, t)

    //ArrayScan.main(args)
    //println(scan_left(a2t(0, a.length), 0, plus))
    val N = a.length
    var t = new Array[Int](N)

    //need to parallelize -- easy -- done
    var c = new Array[Char](N)
    var col = roundUp(N.toDouble / Trees.threshold) // 3
    val threads =
      for (i <- 0 to Trees.threshold)
        yield new Thread {
          override def run(): Unit = {
            for (j <- col * i to math.min((i + 1) * col, N - 1))
              if (a(j) + b(j) > 9)  c(j) = 'C' else if (a(j) + b(j) == 9) c(j) = 'M' else c(j) = 'N'
          }
        }
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())


    //need to parallelize -- hard -- done
    var prefix_scan = new Array[Char](N)
    Trees.scanAPar(c, 'M', f, prefix_scan)
    /*prefix_scan(0) = c(0)
    for (i <- 1 until N)
      prefix_scan(i) = f(prefix_scan(i - 1), c(i))*/

    //need to parallelize -- easy -- done
    var carries = new Array[Int](N)
    carries(0) = 0
    val threads1 =
      for (i <- 0 to Trees.threshold)
        yield new Thread {
          override def run(): Unit = {
            for (j <- col * i to math.min((i + 1) * col, N - 1))
              if (j != 0) carries(j) = bool2int(prefix_scan(j - 1) == 'C')
          }
        }
    threads1.foreach(t => t.start())
    threads1.foreach(t => t.join())


    //need to parallelize -- easy -- done
    var sum = new Array[Int](N)
    val threads2 =
      for (i <- 0 to Trees.threshold)
        yield new Thread {
          override def run(): Unit = {
            for (j <- col * i to math.min((i + 1) * col, N - 1))
              sum(j) = (a(j) + b(j) + carries(j)) % 10
          }
        }
    threads2.foreach(t => t.start())
    threads2.foreach(t => t.join())
    sum
  }
  def f(c1 : Char, c2 : Char) : Char = if (c2 == 'M') c1 else c2
  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
