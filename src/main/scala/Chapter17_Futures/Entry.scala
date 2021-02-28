package Chapter17_Futures

import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.Try

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

  /*
  EXERCISES 4
  Write a function doTogether that, given two functions
  f: T => Future[U] and
  g: U => Future[V],
  produces a function T => Future[(U, V)],
  running the two computations in parallel and, for a given t, eventually yielding (f(t), g(t)).
   */

  def f4 [T, U, V] (f : T => Future[U], g : T => Future[V]) : T => Future[(U,V)] = {
    (t : T) => {
      val functionF = f(t) // using val, the computation for the future start.
      val functionG = g(t)
      functionF.flatMap(u => functionG.map(v => (u, v))) // retrieve result in tuple
    }
  }

  def f4_1 : Int => Future[Int] = {
    x => Future {
      Thread.sleep(2)
      println("f4_1 is done. This should be after f4_2.")
      13
    }
  }

  def f4_2 : Int => Future[Int] = {
    x => Future {
      Thread.sleep(1)
      println("f4_2 is done despite f4_1 is called first.")
      23
    }
  }

  def doTogether(x: Int) : Future[(Int, Int)] = f4(f4_1, f4_2)(1)

  println(Await.result(doTogether(3), 5.seconds))

  /*
  EXERCISES 5
  Write a function that receives a sequence of futures and returns a future that eventually yields a sequence of all results.
   */
  def f5 [T] (fs : Seq[Future[T]]) : Future[Seq[T]] = {
    val resultList = Future { List[T]() }
    fs.foldRight(resultList)( // fs is a Seq of futures. initial result is a empty list
      (fn, fnplus1) => // for two futures,
        fn.flatMap(yn => // get the result from the first futures
          fnplus1.map(ynplus1 => // get the result from the second futures
            yn :: ynplus1))// for each pair of futures, concatenate into the list.
    )
  }

  // defining some futures for testing
  // using def so that future do not get executed immediately.
  def f5_1 : Future[Int] = Future {1}
  def f5_2 : Future[Int] = Future {2}
  def f5_3 : Future[Int] = Future {3}

  def listOfFutures : List[Future[Int]] = List(f5_1, f5_2, f5_3)

  println(Await.result(f5(listOfFutures), 5.seconds))
  /*
  EXERCISES 6
  Write a method
    Future[T] repeat(action: => T, until: T => Boolean)
  that asynchronously repeats the action until it produces a value that is accepted by the until predicate,
  which should also run asynchronously.
  Test with a function that reads a password from the console,
  and a function that simulates a validity check by sleeping for a second and then
  checking that the password is "secret". Hint: Use recursion.
   */


  /*
  EXERCISES 7
  Write a program that counts the prime numbers between 1 and n,
  as reported by BigInt.isProbablePrime.
  Divide the interval into p parts, where p is the number of available processors.
  Count the primes in each part in concurrent futures and combine the results.
   */
  def f7 (n: Int) : Int = {
    val p = Runtime.getRuntime.availableProcessors()
    println("number of processors/parts : " + p)

    def isPrime (part: Seq[Int]) : Future[Int] = { // filter out non-prime and count
      Future {
        part.count(BigInt(_).isProbablePrime(5)) // larger certainty more accurate but longer time.
      }
    }

    // do range and spilt into p parts
    val parts = (1 to n)
      .grouped(p)
      .map(isPrime(_))
      .map(Await.result(_, 1.seconds))

    parts.sum
  }

  println("result : " + f7(1000))

  /*
  EXERCISES 8
  Write a program that asks the user for a URL,
  reads the web page at that URL,
  and displays all the hyperlinks.
  Use a separate Future for each of these three steps.

  EXERCISES 9
  Write a program that asks the user for a URL, reads the web page at that URL,
  finds all the hyperlinks, visits each of them concurrently,
  and locates the Server HTTP header for each of them.
  Finally, print a table of which servers were found how often.
  The futures that visit each page should return the header.

  EXERCISES 10
  Change the preceding exercise where the futures that visit each header update a shared Java ConcurrentHashMap
  or Scala TrieMap. This isn’t as easy as it sounds. A threadsafe data structure is safe in the sense that you
  cannot corrupt its implementation,
  but you have to make sure that sequences of reads and updates are atomic.

  EXERCISES 11
  Using futures, run four tasks that each sleep for ten seconds and then print the current time.
  If you have a reasonably modern computer,
  it is very likely that it reports four available processors to the JVM,
  and the futures should all complete at around the same time.
  Now repeat with forty tasks.
  What happens? Why? Replace the execution context with a cached thread pool.
  What happens now? (Be careful to define the futures after replacing the implicit execution context.)

  EXERCISES 12
  Write a method that, given a URL, locates all hyperlinks, makes a promise for each of them,
  starts a task in which it will eventually fulfill all promises,
  and returns a sequence of futures for the promises.
  Why would it not be a good idea to return a sequence of promises?
   */

  /*
  EXERCISES 13
  Use a promise for implementing cancellation. Given a range of big integers,
  split the range into subranges that you concurrently search for palindromic primes.
  When such a prime is found, set it as the value of the future.
  All tasks should periodically check whether the promise is completed,
  in which case they should terminate.


   */



}
