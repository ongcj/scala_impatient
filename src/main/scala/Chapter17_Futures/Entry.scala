package Chapter17_Futures

import javax.swing.plaf.FontUIResource

import scala.concurrent.{Await, Future}

object Entry extends App {

  /*
  A block of code wrapped in a Future { ... } executes concurrently
  A future succeeds with a result or fails with an exception.
  You can wait for a future to complete, but you don't usually want to.
  You can use callbacks to get notified when a future completes, but that get tedious when chaining callbacks.
  A promises has a future whose value can be set (once), which gives added flexibility for implementing tasks that produce results.
  Pick an execution context that is suitable for the concurrent workload of your computation.
   */

  /*
  EXERCISES 1
  Consider the expression
    for (n1 <- Future { Thread.sleep(1000) ; 2 }

        n2 <- Future { Thread.sleep(1000); 40 })

      println(n1 + n2)


   1. How is the expression translated to map and flatMap calls?
   2. Are the two futures executed concurrently or one after the other?
   3. In which thread does the call to println occur?
   */

  /*
  1. Future { Thread.sleep(1000); 2 }.flatMap( n1 => Future { Thread.sleep(1000); 40 }.map(n2 => println(n1 + n2)))
  2. one after the other.
  3. second thread.
   */


  /*
  EXERCISES 2
  Write a function doInOrder that,
  given two functions f: T => Future[U]
  and g: U => Future[V],
  produces a function T => Future[U] that,
  for a given t, eventually yields g(f(t)).
   */

  import scala.concurrent.ExecutionContext.Implicits.global

  def f2_1 : Int => Future[Int] = {
    x => Future { x + 100 }
  }

  def f2_2 : Int => Future[Int] = {
    x => Future { x * 2 }
  }

  def f2 [T, U, V] (f2_1 : T => Future[U], f2_2 : U => Future[V]) : T => Future[V] = {
    (t: T) => {
      f2_1(t).flatMap(u => f2_2(u)) // flatMap so that we do not have nested Future. This will result in Future[V].
    }
  }

  def doInOrder(x: Int) : Future[Int] = f2(f2_1, f2_2)(x)

  import scala.concurrent.duration._
  println(Await.result(doInOrder(1), 5.seconds))

  /*
  EXERCISES 3
  Repeat the preceding exercise for any sequence of functions of type T => Future[T].
   */

  def f3 [T] (fs : (T => Future[T])*) : T => Future[T] = {
    // fs is a collection of function that is T => Future[T]
      // for value of t, do all the task 1 by 1, compose them in order through reduceLeft.
      fs.reduceLeft(
        (fn, fnplus1) => // for two function that is T => Future[T], fn and fn+1
          (t => fn(t).flatMap(fnplus1)) // reduce to t => fn(t).flatMap(fnplus1) where t is T and fn(t).flatMap(fnplus1) is Future[T]
      ) // reduceLeft combine multiple T => Future[T] to a single T => Future[T]

  }

  def f3_1 : Int => Future[Int] = {
    x => Future { x + 1}
  }

  // f2_1 is + 100, f2_2 is * 2, and f3_1 is + 1. Chaining them would be ((x + 100) * 2) + 1
  def doAllInOrder(x : Int) : Future[Int] = f3(f2_1, f2_2, f3_1)(x)
  println(Await.result(doAllInOrder(1), 5.seconds)) // ((1 + 100) * 2) + 1 = 203


}
