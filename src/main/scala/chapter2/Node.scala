package chapter2

import scala.collection.mutable.Map

/**
 * Created by mazinghafadgi on 13/10/2015.
 */
case class Node2(var i: Int) {
  var next: Node2 = null


   def length():Int = {
     def go(n:Node2, l:Int):Int ={
       if(n.next == null) l else go(n.next, l + 1)
     }
     go(this, 1)

   }

  /**
   *
   * @param value - the value
   */
  def append(value: Int) = {
    var n: Node2 = this
    while (n.next != null) {
      n = n.next
    }
    n.next = Node2(value)
  }

  /**
   * deleteFromTheMiddle
   * @return
   */
  def deleteFromTheMiddle(): Node2 = {
    def go(n: Node2, c: Int): Node2 = {
      if (n != null) go(n.next, c + 1)
      else middle(this, 1, (c / 2))
    }
    def middle(n: Node2, c: Int, m: Int): Node2 = {
      if (c == m) {
        println("middle", m)
        n.next = n.next.next;
        middle(n, c + 1, m)
      }
      if (n.next == null) return this
      middle(n.next, c + 1, m)
    }
    if (this != null) go(this, 0) else this
  }

  /**
   * fold
   * @param acc - the acc
   * @param n - the n
   * @return
   */
  def fold(acc: Int, n: Node2): Int = {
    def go(acc: Int, n: Node2): Int = {
      if (n.next == null) n.i + acc
      else go(acc + n.i, n.next)
    }
    go(acc, n)
  }


  /**
   * deleteDuplicate
   * @param head - the head
   * @return
   */
  def deleteDuplicate(head: Node2): Node2 = {
    def go(n: Node2, myMap: Map[Int, Int]): Node2 = {
      if (n.next == null) return head
      myMap.get(n.next.i) match {
        case Some(x) => println("Some ", x); n.next = n.next.next; go(n, myMap)
        case None => println("None ", n.next.i); myMap.put(n.next.i, n.next.i); go(n.next, myMap)
      }
    }
    val myMap = Map[Int, Int]()
    myMap.put(head.i, head.i)
    go(head, myMap)
  }

  /**
   * contains
   * @param e
   * @return
   */
  def contains(e: Int): Boolean = {
    def go(n: Node2): Boolean = {
      if (n.i == e) return true
      if (n.next == null) return false else go(n.next)
    }
    go(this)
  }


  /**
   * nthToLast
   * @param nth
   * @return
   */
  def nthToLast(nth: Int): Node2 = {
    def go(n: Node2, counter: Int): Node2 = {
      if (counter == nth) return n
      go(n.next, counter + 1)
    }
    go(this, 0)
  }

  /**
   * delete2
   * @param d
   * @return
   */
  def delete2(d: Int): Node2 = {

    def go(n: Node2): Node2 = {
      if (n.next.i == d) {
        n.next = n.next.next
        return this
      }
      go(n.next)
    }

    if (this.i == d) return this
    go(this)
  }

  /**
   * foreach
   * @param f
   * @return
   */
  def foreach(f: Int => Unit): Unit = {

    def go(n: Node2): Unit = {
      if (n != null)
        f(n.i);
      if (n.next != null) go(n.next)
    }
    go(this)
  }
  override def toString = "calling toString"

}

object Node2 {
  def main(args: Array[String]) = {

    val n = Node2(1)
    n.append(2)
    n.append(3)
    n foreach{ x => println(x) }
 }
}
