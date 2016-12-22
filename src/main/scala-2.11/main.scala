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
import scala.io.Source
import scala.reflect.io.File




object main {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 6,
    Key.exec.maxWarmupRuns -> 6,
    Key.exec.benchRuns -> 5,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)

  val r = scala.util.Random
  val file = "log.txt"
  val MaxTime = 1e9.toInt
  val K       = 10
  val M       = 20
  val N       = 2
  val FreePlace = new Client(MaxTime)
  var close = false

  var sofa = new Array[Client](K)
  var clerk   = new Clerk

  class Client(private var time: Int, private var place: Int = -1){
    def getTime() = time
    def take_place(): Int =
      this.synchronized {
          clerk.synchronized {
            var i = clerk.find_place()
            if (i == -1) {
              appendToFile(file, s"C$time leave\n")
              -1
            } else {
              appendToFile(file, s"C$time seat in $i place\n")
              sofa(i) = this
              place = i
              i
            }
          }
      }

    def free(): Unit = {
      this.synchronized {
        sofa(place) = FreePlace
      }
    }
  }


  class Barber(private val num: Int) {
    def getNum() = num
    def cut (target: Client) =
      this.synchronized {
        target.synchronized {
          Thread.sleep(r.nextInt(200) + 100)
          if (deb)
            print(num + ": ")
          printSofa()
          val tmp = this.getNum()
          appendToFile(file, s"B$tmp is free\n")
          this.find_client()
        }
      }
    def find_client(): Unit =
      this.synchronized {
        if (!close) {
          var target = clerk.find_next_clien()
          if (target.getTime() != MaxTime) {
            target.synchronized {
              val t = new Thread {
                override def run {
                  target.free()
                }
              }
              val tmp = this.getNum()
              appendToFile(file, s"C${target.getTime()} -> B$tmp\n")
              t.run
              this.cut(target)
              t.join()
            }
          }
          else {
            appendToFile(file, s"B$num can't find client\n")
            Thread.sleep(r.nextInt(10))
            this.find_client()
          }
        }
      }
  }

  class Clerk() {
    def find_next_clien(): Client =
      this.synchronized {
        var t = new Client(MaxTime)
        if (!close) {
          for (i <- 0 until K) {
            if (sofa(i).getTime() < t.getTime()) {
              t = sofa(i)
            }
          }
        }
        t
      }
    def find_place(): Int =
      this.synchronized {
        var t = -1
        if (!close) {
          for (i <- 0 until K)
            if (sofa(i).getTime() == MaxTime) {
              t = i
            }
          }
        t
      }
  }

  def visit(client: Client) = {
    val t = new Thread {
      override def run() {
        client.take_place()
      }
    }
    t.start()
    Thread.sleep(r.nextInt(100))
    t
  }

  def pickUp(barber: Barber) = this.synchronized{
    val t = new Thread {
      override def run() {
        barber.find_client()
      }
    }
    t.start()
    t
  }

  val deb = false

  def printSofa(): Unit = {
    if (deb) {
      var j = 0
      for (i <- 0 until K)
        if (sofa(i).getTime() == MaxTime) {
          j = j + 1
          print("0 ")
        }
        else {
          print(sofa(i).getTime() + " ")
        }
      print(s" -- $j")
      println()
    }
  }

  def main(args: Array[String]): Unit ={

    // (a, b) <+> (a',b') = (a' * a, a' * b + b') -- is asociative
    writeToFile(file, "\n\n")
    sofa = Array.fill(K)(FreePlace)
    var arr: Array[Client] = Array()
    for (i <- 0 to M + 1)
      arr = arr :+ new Client(i)

    var arr1: Array[Barber] = Array()
    for (i <- 0 to N + 1)
      arr1 = arr1 :+ new Barber(i)

    var threads1 =
      for (i <- 0 until 0)
        yield pickUp(arr1(i + 1))

    val thread = new Thread {
      override def run {
        threads1 =
          for (i <- 0 until N)
            yield pickUp(arr1(i + 1))
      }
    }

    thread.run

    val threads =
    for (i <- 0 until M)
      yield visit(arr(i + 1))

    print("   ")
    printSofa()



    threads.foreach(t => t.join())
    close = true
    if (deb)
      println("Close!!!")
    threads1.foreach(t => t.join())
    checkLog()


  }

  def checkLog(): Unit = {
    val filename = "log.txt"
    var t = 0
    var arr = new Array[Boolean](N)
    var places = new Array[Int](M)
    for (line <- Source.fromFile(filename).getLines()) {
      if (line != "") {
        t = t + 1
        if (t > 10) {
         // return
        }

        var tp = line(0)
        var i = 0
        do
          i = i + 1
        while (line(i).isDigit)
        var s = ""
        for (j <- 1 until i)
          s = s :+ line(j)

        //println(s"$tp $s ${line(s.length + 2)}")
        var p = line(s.length + 2)
        if (tp == 'B') {
          if (p == 'c') {
            if (arr(s.toInt - 1)) {
              err
            }
          }
          if (p == 'i') {
            if (arr(s.toInt - 1)) {
              arr(s.toInt - 1) = false
            } else
              err
          }
        }
        if (tp == 'C') {
          if (p == '-') {
            var tp2 = line(s.length + 5)
            var s2 = ""
            for (j <- s.length + 6 until line.length)
              s2 = s2 :+ line(j)
            //println(s"$tp$s -> $tp2$s2")
            arr(s2.toInt - 1) = true
            var tmp = -1
            for (j <- 0 until M) {
              if (places(j) == s.toInt) {
                if (tmp == -1)
                  tmp = j
                else
                  err
              }
            }
            if (tmp == -1)
              err
            else
              places(tmp) = 0
          }
          if (p == 's') {
            var s2 = ""
            var j = s.length + 10
            do {
              s2 = s2 :+ line(j)
              j = j + 1
            } while (line(j).isDigit)
            //println(s"$tp$s to $s2")
            if (places(s2.toInt - 1) != 0)
              err
            else
              places(s2.toInt - 1) = s.toInt
          }
        }

      }
    }
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

  def bool2int(f : Boolean) : Int = if (f) 1 else 0
  def roundUp(d: Double) = math.ceil(d).toInt
  def roundDown(d: Double) = math.floor(d).toInt
}
