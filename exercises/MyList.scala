package exercises

import javafx.collections.transformation.SortedList

abstract class MyList[+A]{

  /*
    head = first element of the list
    tail = remainder of the list
    isEmpty = is this list empty
    add(int) => new list with this element added
    toString => a string representation of the list
   */

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](x: B): MyList[B]
  def printElements: String
  override def toString: String = "[" + printElements + "]"

  //higher-order function
  def map[B](transformer: A => B ):MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  //concatenation
  def ++[B >: A](list: MyList[B]):MyList[B]
  //hofs
  def foreach(f: A => Unit): Unit
  def sort(compare: (A, A) => Int): MyList[A]
  def zipwith[B, C](list: MyList[B], zip: (A,B) => C): MyList[C]
  def fold[B](start: B)(operator: (B,A)=> B): B
}

case object Empty extends MyList[Nothing]{
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](x: B): MyList[B] = new Cons(x, Empty)
  def printElements: String = " "

  def map[B](transformer: Nothing => B):MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]):MyList[B] = list

  def foreach(f: Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing) => Int): MyList[Nothing] = Empty

  def zipwith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] =
    if (!list.isEmpty) throw new RuntimeException("List do not have same length")
    else Empty

  override def fold[B](start: B)(operator: (B, Nothing) => B): B = start
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](x: B): MyList[B] = new Cons(x, this)
  def printElements: String = {
    if (t.isEmpty) "" + h
    else h + " " + t.printElements
  }

  /*
  for filter
      [1, 2, 3].filter(n%2 ==0) =
         [2, 3].filter(n%2 ==0) =
         = new Cons(2, [3].filter(n%2==0))
         = new Cons(2, Empty.filter(n%2==0))
         = new Cons(2, Empty)
   */
  def filter(predicate: A => Boolean): MyList[A] =
    if(predicate.apply(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  /*
      For map
      [1, 2, 3].map(n * 2)
      = new Cons(2, [2,3].map(n * 2))
      = new Cons(2, new Cons(4, [3].map(n * 2))
      = new Cons(2, new Cons(4, new Cons(6, Empty.map(n * 2))))
      = new Cons(2, new Cons(4, new Cons(6, Empty))))
   */
  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer.apply(h), t.map(transformer))

  /*
      [1,2] ++ [3,4,5]
      =new Cons(1, [2] ++ [3,4,5]
      =new Cons(1,new Cons(2, Empty ++ [3,4,5]))
      =new Cons(1,new Cons(2, new Cons(3, new Cons(4, new Cons(5)))))
   */
  def ++[B >: A](list: MyList[B]):MyList[B] = new Cons(h, t ++ list)

  /*
      [1,2].flatMap(n=>[n, n+1])
      = [1,2] ++ [2].flatMap(n => [n, n+1])
      = [1,2] ++ [2,3] ++ Empty.flatMap(n => [n, n+1])
      = [1,2] ++ [2,3] ++ Empty
      = [1 ,2, 2, 3]
   */
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer.apply(h) ++ t.flatMap(transformer)

  //hof
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A, A) => Int): MyList[A] = {
    def insert(x: A, sortedList: MyList[A]): MyList[A] = {
      if (sortedList.isEmpty) new Cons(x, Empty)
      else if (compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))
    }

    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipwith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("Lists do not have same length")
    else new Cons(zip(h, list.head),t.zipwith(list.tail,zip))

  /*
      [1,2,3].fold(0)(+)
      = [2,3].fold(1)(+)
      = [3].fold(3)(+)
      = [0].fold(6)(+)
      6
   */
  def fold[B](start: B)(operator: (B, A) => B): B = {
    val newStart = operator(start, h)
    t.fold(newStart)(operator)
  }
}

object ListTest extends App {
//  val list = new Cons(1, new Cons(2, new Cons(3, Empty)))
//  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("Scala", Empty))
//  println(list.tail.head)
//  println(list.add(4).head)
//  println(list.isEmpty)
//  println(list.toString)
//  println(listOfStrings.toString)

  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val cloneList: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listInt: MyList[Int] = new Cons(4, new Cons(5, Empty))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("Scala", Empty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(elem => elem * 2).toString)

  println {
    listOfIntegers.filter(elem => elem % 2 == 0).toString

    println((listOfIntegers ++ listInt).toString)
    println(listOfIntegers.flatMap(elem => new Cons(elem, new Cons(elem + 1, Empty))).toString)
    println(listOfIntegers == cloneList)
  }

  listOfIntegers.foreach(x => println(x))
  println(listOfIntegers.sort((x,y)=>y-x))
  println(listInt.zipwith[String, String](listOfStrings, _ + " " + _))

  println(listOfIntegers.fold(0)(_ + _))

}




