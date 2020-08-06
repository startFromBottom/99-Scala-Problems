
object P01 {

  // P01(*) Find the last element of a list

  // 1) throw exception
  @scala.annotation.tailrec
  def last[A](l: List[A]): A = l match {
    case h :: Nil => h
    case _ :: t => last(t)
    case _ => throw new NoSuchElementException
  }

  // 2) Option type
  @scala.annotation.tailrec
  def last_1[A](l: List[A]): Option[A] = l match {
    case h :: Nil => Option(h)
    case _ :: t => last_1(t)
    case _ => None
  }

}

object P02 {
  // 2번부터는 예외를 던지는 방향으로 통일
  // P02(*) Find the last but one element of a list

  @scala.annotation.tailrec
  def penultimate[A](l: List[A]): A = l match {
    case h1 :: (h2 :: Nil) => h1
    case _ :: t => penultimate(t)
    case _ => throw new NoSuchElementException
  }

}

object P03 {

  // P03(*) Find the Kth element of a list
  // By convention, the first element in the list is element 0

  def nth[A](n: Int)(ls: List[A]): A = {

    @scala.annotation.tailrec
    def go(n: Int, l: List[A]): A =
      if (n == 0) l.head
      else go(n - 1, l.tail)

    go(n, ls)

  }

}

object P04 {

  // P04(*) Find the number of elements of a list

  def length[A](ls: List[A]): Int = {
    @scala.annotation.tailrec
    def go(length: Int, l: List[A]): Int = l match {
      case Nil => length
      case _ :: t => go(length + 1, t)
    }

    go(0, ls)
  }

}

object P05 {

  // P05(*) Reverse a List

  def reverseViaFoldLeft[A](ls: List[A]): List[A] =
    ls.foldLeft(List[A]())((t, h) => h :: t)


  def reverseViaRec[A](ls: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go(left: List[A], right: List[A]): List[A] = {
      // pattern-match로 해도 좋음
      if (right.isEmpty) left
      else go(right.head :: left, right.tail)
    }

    go(List(), ls)
  }

}

object P06 {

  // P06(*) Find out whether a list is a palindrome

  def isPalindrome[A](ls: List[A]): Boolean =
    ls == P05.reverseViaFoldLeft(ls)

}

object P07 {

  // P07(**) Flatten a nested list structure

  def flatten(ls: List[Any]): List[Any] = ls match {
    case (h :: t) :: l => (h :: t) ::: flatten(l)
    case h :: t => h :: flatten(t)
    case _ => Nil
  }

  def flattenViaFlatMap(ls: List[Any]): List[Any] =
    ls flatMap({
      case l: List[_] => flattenViaFlatMap(l)
      case e => List(e)
    })

}

