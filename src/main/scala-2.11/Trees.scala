package object Trees {

  var threshold = 2

  def foldASegSeq[A,B](inp: Array[A],
                       left: Int, right: Int,
                       b0: B, // initial element
                       f: (B,A) => B): B = {
    var b= b0
    var i= left
    while (i < right) {
      b= f(b, inp(i))
      i= i+1
    }
    b
  }

  // Binary trees whose nodes store elements of type A
  sealed abstract class FoldTree[A] {
    val res: A // whether it is leaf or internal node, res stores the result
  }
  case class Leaf[A](from: Int, to: Int, resLeaf: A) extends FoldTree[A] {
    val res= resLeaf
  }
  case class Node[A](l: FoldTree[A], r: FoldTree[A], resNode: A) extends FoldTree[A] {
    val res= resNode
  }

  def upsweep[A](inp: Array[A],
                 left: Int, right: Int,
                 a0: A,
                 f: (A,A) => A): FoldTree[A] = {
    // requires f to be associative
    if (right - left < threshold)
      Leaf(left, right, foldASegSeq(inp, left + 1, right, inp(left), f))
    else {
      val mid = left + (right - left)/2

      var t1 : FoldTree[A] = Leaf(0, 0, a0)
      var t2 : FoldTree[A] = Leaf(0, 0, a0)
      val Task = new Thread {
        override def run(): Unit = {
          t1 = upsweep(inp, left, mid, a0, f)
        }
      }
      Task.run()
      t2 = upsweep(inp, mid, right, a0, f)
      Task.join()
      Node(t1, t2, f(t1.res,t2.res))
    }
  }

  def scanASegSeq1[A](inp: Array[A],
                      left: Int, right: Int,
                      a0: A,
                      f: (A,A) => A,
                      out: Array[A]) = {
    if (left < right) {
      var i = left
      var a = a0
      while (i < right) {
        a = f(a, inp(i))
        out(i) = a
        i = i + 1
      }
    }
  }

  def downsweep[A](inp: Array[A],
                   a0: A,
                   f: (A,A) => A,
                   t: FoldTree[A],
                   out: Array[A]): Unit = {
    t match {
      case Leaf(from, to, res) =>
        scanASegSeq1(inp, from, to, a0, f, out)
      case Node(l, r, res) => {
        val Task = new Thread {
          override def run(): Unit = {
            downsweep(inp, a0, f, l, out)
          }
        }
        Task.run()
        downsweep(inp, f(a0,l.res), f, r, out)
        Task.join()
      }
    }
  }

  def scanASegPar[A](inp: Array[A],
                     from: Int, to: Int,
                     a0: A,
                     f: (A,A) => A,
                     out: Array[A]) = {
    val t = upsweep(inp, from, to, a0, f)
    downsweep(inp, a0, f, t, out)
  }

  def scanAPar[A](inp: Array[A],
                  a0: A,
                  f: (A,A) => A,
                  out: Array[A]) = {
    //out(0)= a0
    scanASegPar(inp, 0, inp.length, a0, f, out)
  }

}
