package Chapter14_PatternMatchingAndCaseClass

object Entry extends App {

  /*
  EXERCISES 2
   Using pattern matching, write a function swap that receives a pair of integers and returns the pair with the components swapped.
   */

  def f2 (pair: (Int, Int)) : (Int, Int) = {
    pair match {
      case (a, b) => (b, a)
    }
  }

  println(f2(1, 2))

  /*
  EXERCISES 3
  Using pattern matching, write a function swap that swaps the first two elements of an array provided its length is at least two.
   */

  def f3 (array : Array[Int]) : Array[Int] = {
    array match {
      case Array(a, b, tail @ _*) => Array(b, a) ++ tail.toArray[Int]
    }
  }

  f3(Array(1, 2, 3, 4, 5, 6, 7)).foreach(print)
  println("")

  /*
  EXERCISES 4
  Add a case class Multiple that is a subclass of the Item class.
  For example, Multiple(10, Article("Blackwell Toaster", 29.95)) describes ten toasters.
  Of course, you should be able to handle any items, such as bundles or multiples,
  in the second argument. Extend the price function to handle this new case.
   */

  abstract class Item {}
  case class Article (desc: String, price: Double) extends Item {}
  case class Bundle (desc: String, discount: Double, items: Item*) extends Item {}
  case class Multiple(num: Int, item: Item) extends Item {}

  // function that computes the price of an item
  def price(it: Item): Double = {
    it match {
      case Article(_, p) => p
      case Bundle(_, d, its @ _*) => its.map(price).sum - d
      case Multiple(num, item) => num * price(item)
    }
  }
  println(price(Multiple(10, Article("Blackwell Toaster", 29.95))))

  /*
  EXERCISES 5
  One can use lists to model trees that store values only in the leaves.
  For example, the list ((3 8) 2 (5)) describes the tree
       •
      /|\
     • 2 •
    / \  |
    3 8  5
  However, some of the list elements are numbers and others are lists.
  In Scala, you cannot have heterogeneous lists, so you have to use a List[Any].
  Write a leafSum function to compute the sum of all elements in the leaves,
  using pattern matching to differentiate between numbers and lists.
   */

  // using pattern matching to add all leaf nodes..
  def f5(tree: List[Any]): Int = {
    tree match {
      case Nil => 0
      // leaf node will be in the most inner of the List. If it is not the most inner, keep on matching.

      // visit left first then right
      case (left: List[Any]) :: (x: Int) :: (right: List[Any]) :: Nil => f5(left) + f5(right)

      // visit leafs
      case (x: Int) :: (y: Int) :: Nil => x + y

      // case when only one leaf node from parent node
      case (x: Int) :: Nil => x

    }
  }
  val tree = List(List(3, 8), 2, List(5))
  println(f5(tree))

  /*
  EXERCISES 6
  A better way of modeling such trees is with case classes. Let’s start with binary trees.

    sealed abstract class BinaryTree

    case class Leaf(value: Int) extends BinaryTree

    case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  Write a function to compute the sum of all elements in the leaves.
   */

  sealed abstract class BinaryTree

  case class Leaf(value: Int) extends BinaryTree

  case class Node(left: BinaryTree, right: BinaryTree) extends BinaryTree

  def f6(tree: BinaryTree) : Int = {
    tree match {
      case l: Leaf => l.value
      case n: Node => f6(n.left) + f6(n.right)
    }
  }
  val binTree = Node(
    Node(Leaf(3), Leaf(8)),
    Leaf(5)
  )
  println(f6(binTree))

  /*
  EXERCISES 7
  Extend the tree in the preceding exercise so that each node can have an arbitrary number of children,
  and reimplement the leafSum function.
  The tree in Exercise 5 should be expressible as
    Node(Node(Leaf(3), Leaf(8)), Leaf(2), Node(Leaf(5)))
   */
  sealed abstract class BinaryTree_7

  case class Leaf_7(value: Int) extends BinaryTree_7

  case class Node_7(children : BinaryTree_7*) extends BinaryTree_7

  def f7 (tree: BinaryTree_7) : Int = {
    tree match {
      case Leaf_7(value) => value
      case Node_7(children @ _*) => children.map(f7).sum
    }
  }
  val tree_7 = Node_7(Node_7(Leaf_7(3), Leaf_7(8)), Leaf_7(2), Node_7(Leaf_7(5)))
  println(f7(tree_7))

  /*
  EXERCISES 8
  Extend the tree in the preceding exercise so that each nonleaf node stores an operator in addition to the child nodes.
  Then write a function eval that computes the value. For example, the tree
             +

            /|\

          *  2  -

        / \    |

        3   8   5
   has value (3 × 8) + 2 + (–5) = 21.

   Pay attention to the unary minus.
   */
  sealed abstract class BinaryTree_8

  case class Leaf_8(value: Int) extends BinaryTree_8

  case class Node_8(operator: Char, children : BinaryTree_8*) extends BinaryTree_8

  def f8 (tree: BinaryTree_8) : Int = {
    tree match {
      case Leaf_8(value) => value
      case Node_8(operator, children @ _*) => {
        operator match {
          case '+' => children.map(f8).sum
          case '-' => children.map(f8).foldLeft(0)(_ - _)
          case '*' => children.map(f8).product
        }
      }
    }
  }
  val expr = Node_8('+', Node_8('*', Leaf_8(3), Leaf_8(8)), Leaf_8(2), Node_8('-', Leaf_8(5)))
  println(f8(expr))

  /*
  EXERCISES 9
  Write a function that computes the sum of the non-None values in a List[Option[Int]]. Don’t use a match statement.
   */

  def f9 (list: List[Option[Int]]): Int = {
    list.map(v => v.getOrElse(0)).sum
  }
  println(f9(List(None, Some(5), None, Some(6))))

  /*
  EXERCISES 10
  Write a function that composes two functions of type Double => Option[Double],
  yielding another function of the same type.
  The composition should yield None if either function does. For example,

    def f(x: Double) = if (x != 1) Some(1 / (x - 1)) else None

    def g(x: Double) = if (x >= 0) Some(sqrt(x)) else None

    val h = compose(g, f) // h(x) should be g(f(x))

  Then h(2) is Some(1), and h(1) and h(0) are None.
   */
  def f(x: Double) : Option[Double] = if (x != 1) Some(1 / (x - 1)) else None

  def g(x: Double) : Option[Double] = if (x >= 0) Some(math.sqrt(x)) else None
  // this f9 function is a compose of f and g, such that h(x) = g(f(x)).
  def f9 (g : Double => Option[Double], f: Double => Option[Double]) : Double => Option[Double] = {
    (x: Double) => {
      f(x) match {
        case None => None
        case Some(d) => g(d)
      }
    }
  }

  val h = f9(g, f)
  println(h(2))
  println(h(1))
  println(h(0))

}
