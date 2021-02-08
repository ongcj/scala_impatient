package Chapter13_Collections

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Entry extends App {
  /*
  EXERCISES 1
  Write a function that, given a string, produces a map of the indexes of all characters.
  For example, indexes("Mississippi") should return a map associating 'M' with the set {0}, 'i' with the set {1, 4, 7, 10},
  and so on. Use a mutable map of characters to mutable sets.
  How can you ensure that the set is sorted?
   */

  def f1 (s: String) : scala.collection.mutable.Map[Char, scala.collection.mutable.LinkedHashSet[Int]] = {
    /*
    A linked hash set remembers the order in which elements were inserted. It keeps a linked list for this purpose.
     */
    val map = new scala.collection.mutable.LinkedHashMap[Char, scala.collection.mutable.LinkedHashSet[Int]]
    @scala.annotation.tailrec
    def recursiveHelper(head: Char, tail: String, index: Int, map: scala.collection.mutable.LinkedHashMap[Char, scala.collection.mutable.LinkedHashSet[Int]]): scala.collection.mutable.LinkedHashMap[Char, scala.collection.mutable.LinkedHashSet[Int]] = {
      map(head) = map.getOrElse(head, new mutable.LinkedHashSet[Int]) + index
      if (tail.isEmpty) {
        map
      }
      else {
        recursiveHelper(tail.head, tail.tail, index + 1, map)
      }
    }
    recursiveHelper(s.head, s.tail, 0, map)
    }

  println(f1("Mississippi"))

  /*
  EXERCISES 2
  Repeat the preceding exercise, using an immutable map of characters to lists.
   */
  def f2 (s: String) : Map[Char, List[Int]] = {
    val map = Map[Char, List[Int]]()
    @scala.annotation.tailrec
    def recursiveHelper(head: Char, tail: String, index: Int, map: Map[Char, List[Int]]): Map[Char, List[Int]] = {
      if (tail.isEmpty) {
        map + (head -> (map.getOrElse(head, List.empty) :+ index) )
      }
      else {
        recursiveHelper(tail.head, tail.tail, index + 1, map + (head -> (map.getOrElse(head, List.empty) :+ index) ))
      }
    }
    recursiveHelper(s.head, s.tail, 0, map)
  }
  println(f2("Mississippi"))

  /*
  EXERCISES 3
  Write a function that removes every second element from a ListBuffer.
  Try it two ways.
  Call remove(i) for all even i starting at the end of the list.
  Copy every second element to a new list.
  Compare the performance.
   */

  /*
  ListBuffer is backed by a linked list with a reference to the last node.
  This makes it efficient to add or remove elements at either end of the list.
   */
  def f3_1 (lb: ListBuffer[Int]) : ListBuffer[Int] = {
    // Call remove(i) for all even i starting at the end of the list.

    @scala.annotation.tailrec
    def recursiveHelper(index: Int, lb: ListBuffer[Int]) : ListBuffer[Int] = {
      if (index < 0) {
        lb
      }
      else {
        if (index % 2 == 0) {
          lb.remove(index)
          recursiveHelper(index - 1, lb)
        }
        else {
          recursiveHelper(index - 1, lb)
        }
      }

    }
    recursiveHelper(lb.length - 1, lb)
  }
  println(f3_1(ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

  def f3_2 (lb: ListBuffer[Int]) : ListBuffer[Int] = {
    // Copy every second element to a new list.

    @scala.annotation.tailrec
    def recursiveHelper(index : Int, lb : ListBuffer[Int], result: ListBuffer[Int]) : ListBuffer[Int] = {
      if (lb.length <= index) {
        result
      }
      else {
        if (index % 2 != 0) {
          recursiveHelper(index + 1, lb, result += lb(index))
        }
        else {
          recursiveHelper(index + 1, lb, result)
        }
      }
    }
    recursiveHelper(0, lb, ListBuffer[Int]())
  }
  println(f3_2(ListBuffer(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))


  /*
  EXERCISES 4
  Write a function that receives a collection of strings and a map from strings to integers.
  Return a collection of integers that are values of the map corresponding to one of the strings in the collection.

  For example, given Array("Tom", "Fred", "Harry") and Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5),
  return Array(3, 5). Hint: Use flatMap to combine the Option values returned by get.
   */

  def f4 (sArray : Array[String], sMap : Map[String, Int]) : Array[Int] = {
    /*
    Yields a collection instead of a single value, you may want to concatenate all result.
     */

    sArray.flatMap(sMap.get)
  }
  val strs = Array("Tom", "Fred", "Harry")
  val map = Map("Tom" -> 3, "Dick" -> 4, "Harry" -> 5)
  f4(strs, map).foreach(x => print(x + " "))
  println("")

  /*
  EXERCISES 5
  Implement a function that works just like mkString, using reduceLeft.
   */
  def f5 (s: Seq[String], sep: String) : String = {
    // reduce the collection from the left.
    s.reduceLeft(_ + sep + _)
  }
  println(f5(List("Foo", "Bar", "Quux"), " | "))


  /*
  EXERCISES 6
  Given a list of integers lst,
  what is
  (lst :\ List[Int]())(_ :: _)?
  (List[Int]() /: lst)(_ :+ _)?
  How can you modify one of them to reverse the list?
   */

  val lst = (1 to 10).toList
  val a = (lst :\ List[Int]())(_ :: _)
  val a_1 = lst.foldRight(List[Int]())(_ :: _) // currying
  // fold right starting with an empty list using :: which adds an element at the beginning of the list.
  println(a)
  println(a_1)
  val b = (List[Int]() /: lst)(_ :+ _)
  val b_1 = (List[Int]()).foldLeft(lst)(_ :+ _)
  // fold left starting with lst using :+ which copy of the empty list with an element appended
  println(b)
  println(b_1)

  /*
  EXERCISES 7
  In Section 13.10, “Zipping,” on page 187, the expression (prices zip quantities) map { p => p._1 * p._2 } is a bit inelegant.
  We can’t do (prices zip quantities) map { _ * _ } because _ * _ is a function with two arguments,
  and we need a function with one argument that is a tuple.
  The tupled method of the Function object changes a function with two arguments to one that takes a tuple.

  Apply tupled to the multiplication function so you can map it over the list of pairs.
   */

  // from 13.10
  val prices = List(5.0, 20.0, 9.95)
  val quantities = List(10, 2, 1)
  val pricesQuantities = prices zip quantities
  println(pricesQuantities)
  println(pricesQuantities map {p => p._1 * p._2}) // inelegant

  // solution
  println(pricesQuantities.map(
    ((_: Double) * (_: Int)).tupled
  ))

  /*
  EXERCISES 8
  Write a function that turns an array of Double values into a two-dimensional array.
  Pass the number of columns as a parameter.

  For example, with Array(1, 2, 3, 4, 5, 6) and three columns,
  return Array(Array(1, 2, 3), Array(4, 5, 6)). Use the grouped method.
   */

  def f8 (a : Array[Double], col : Int) : Array[Array[Double]] = {
    // grouped partitions elements in fixed size iterable collections
    a.grouped(col).toArray
  }
  f8(Array(1, 2, 3, 4, 5, 6), 3).map(row => println(row.mkString(" ")))

  /*
  EXERCISES 9
  The Scala compiler transforms a for/yield expression
    for (i <- 1 to 10; j <- 1 to i) yield i * j
  to invocations of flatMap and map, like this:
    (1 to 10).flatMap(i => (1 to i).map(j => i * j))
  Explain the use of flatMap. Hint: What is (1 to i).map(j => i * j) when i is 1, 2, 3?
    flapMap is used to apply function to a collection and concatenate all result into a collection. Or else it is a nested collection
  What happens when there are three generators in the for/yield expression?
    flatMap, flatMap then map
   */

  /*
  EXERCISES 10
  The method java.util.TimeZone.getAvailableIDs yields time zones such as Africa/Cairo and Asia/Chungking.
  Which continent has the most time zones? Hint: groupBy.
   */
  val time = java.util.TimeZone.getAvailableIDs
  // time.foreach(println)
  val highestTimeZone = time.groupBy(t => t.split("/").head)
    .map(x => (x._1, x._2.length))
      .reduce((a, b) => if (a._2 > b._2) a else b)
  println(highestTimeZone)

  /*
  EXERCISES 11
  Harry Hacker reads a file into a string and wants to use a parallel collection
  to update the letter frequencies concurrently on portions of the string.
  He uses the following code:
    val frequencies = new scala.collection.mutable.HashMap[Char, Int]

    for (c <- str.par) frequencies(c) = frequencies.getOrElse(c, 0) + 1

  Why is this a terrible idea? How can he really parallelize the computation? (Hint: Use aggregate.)
   */
  // The parallel computations mutate shared variable, frequencies. The result is unpredictable.


  val str = "scala is so funnnnn"

  val frequencies = Map[Char, Int]()

  // aggregate applies on operator to parts of the collection, and then uses another operator to combine the results.
  // from the book, the below first uses "_ + _" to add element to the Set[Char], and then uses "_ ++ _" to combine all the set.
//  println(str.par.aggregate(Set[Char]())(_ + _, _ ++ _))

  // Similar, we need a function to count the char frequency and store in a Map.
  def charCounter (charMapping:Map[Char, Int], c: Char) : Map[Char, Int] = {
    charMapping + (c -> (charMapping.getOrElse(c, 0) + 1))
  }
  // next, we need to have a function to correctly merge all the small map.
  @scala.annotation.tailrec
  def mergeMap(a : Map[Char, Int], b : Map[Char, Int]) : Map[Char, Int] = {
    // merge b to a
    if (b.isEmpty) {
      a
    }
    else {
      // check if b.head exist in a
      if (a.contains(b.head._1)) {
        val newCount = a(b.head._1) + b.head._2
        mergeMap(a + (b.head._1 -> newCount), b.tail)
      }
      else {
        mergeMap(a + b.head, b.tail)
      }
    }
  }

  val result10 = str.par.aggregate(Map[Char, Int]())(
    charCounter,
    mergeMap)

  println(result10)


}
