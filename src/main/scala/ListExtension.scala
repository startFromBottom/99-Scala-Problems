import java.util.NoSuchElementException

import javax.xml.crypto.dsig.spec.ExcC14NParameterSpec

import scala.collection.mutable.ListBuffer

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
    ls flatMap ({
      case l: List[_] => flattenViaFlatMap(l)
      case e => List(e)
    })

}


object P08 {

  // P08(**) Eliminate consecutive duplicates of list elements
  def compress[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]())((h, t) => (h, t) match {
      case (h, (h1 :: _)) =>
        if (h == h1) t
        else h :: t
      case (h, Nil) => List(h)
      case _ => Nil
    })


}

object P09 {

  // P09(**) Pack consecutive duplicates of list elements into sublists

  def pack[A](ls: List[A]): List[List[A]] = {
    def go(acc: List[List[A]], cur: List[A], l: List[A]): List[List[A]] = {
      if (l.isEmpty) cur :: acc
      else (cur, l) match {
        case (Nil, (hl :: tl)) => go(acc, hl :: cur, tl)
        case ((h :: t), (hl :: tl)) =>
          if (h == hl) go(acc, h :: cur, tl)
          else go(cur :: acc, hl :: Nil, tl)
      }
    }

    go(List(), List(), ls).reverse
  }

}

object P10 {

  // P10 (*) Run-length encoding of a list
  def encode[A](ls: List[A]): List[(Int, A)] =
    P09.pack(ls) map (l => (l.length, l.head))
}


object P11 {

  // P11 (*) Modified run-length encoding
  def encodedModified[A](ls: List[A]): List[Any] =
    P09.pack(ls) map (l =>
      if (l.length == 1) l.head
      else (l.length, l.head))

}

object P12 {

  // P12 (**) Decode a run-length encoded list
  def decode[A](encoded: List[(Int, A)]): List[A] =
    encoded flatMap (l => List.fill(l._1)(l._2))

}

object P13 {

  // P13 (**) Run-length encoding of a list(direct solution)
  // Implement the so-called run-length encoding data compression
  // method directly(do not use pack, do all the work directly

  def encodeDirect[A](ls: List[A]): List[(Int, A)] = {
    @scala.annotation.tailrec
    def go(acc: List[(Int, A)], cur: (Int, A), l: List[A]): List[(Int, A)] =
      l match {
        case h :: Nil => cur :: acc
        case h1 :: (h2 :: t2) =>
          if (h1 == h2) go(acc, (cur._1 + 1, cur._2), h2 :: t2)
          else go(cur :: acc, (1, h2), h2 :: t2)
      }

    if (ls.isEmpty) Nil
    else go(List(), (1, ls.head), ls).reverse

  }

}

object P14 {

  // P14 (*) Duplicate the elements of a list
  def duplicate[A](ls: List[A]): List[A] = ls match {
    case Nil => Nil
    case h :: t => h :: (h :: duplicate(t))
  }

}

object P15 {

  // P15 (**) Duplicate the elements of a list a given number of times
  def duplicateN[A](n: Int, ls: List[A]): List[A] =
    ls flatMap (v => List.fill(n)(v))

}

object P16 {

  // P16 (**) Drop every Nth element from a list
  def drop[A](n: Int, ls: List[A]): List[A] = {
    @scala.annotation.tailrec
    def go(acc: List[A], cur: Int, ls: List[A]): List[A] = ls match {
      case Nil => acc
      case h :: t =>
        if (cur % n == 0) go(acc, 1, t)
        else go(h :: acc, cur + 1, t)
    }

    go(List(), 1, ls).reverse
  }

  // Functional solution(link : http://aperiodic.net/phil/scala/s-99/p16.scala)
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map (_._1)
}

object P17 {

  // P17 (**) Split a list into two parts
  def split[A](n: Int, ls: List[A]): (List[A], List[A]) =
    ls.splitAt(n)

}

object P18 {

  // P18 (**) Extract a slice from a list
  def slice[A](lo: Int, hi: Int, ls: List[A]): List[A] =
    ls.zipWithIndex.filter { v => v._2 >= lo && v._2 < hi } map (_._1)
}

object P19 {

  // P19 (**) Rotate a list N places to the left
  def rotate[A](n: Int, ls: List[A]): List[A] =
    if (ls.isEmpty) Nil
    else {
      val mod = n % ls.length
      if (mod < 0) (ls drop (mod + ls.length)) ::: (ls take (mod + ls.length))
      else (ls drop mod) ::: (ls take mod)
    }
}

object P20 {

  // P20 (*) Remove the Kth element from a list
  def removeAt[A](pos: Int, ls: List[A]): (List[A], A) = {
    if (pos < 0) throw new Exception("minus pos is not allowed")

    var buf = new collection.mutable.ListBuffer[A]()

    @scala.annotation.tailrec
    def go(right: List[A], i: Int): (List[A], A) =
      (right, i) match {
        case (Nil, _) => throw new Exception("cannot remove")
        case (h :: t, 0) => (t, h)
        case (h :: t, i) =>
          buf += h
          go(t, i - 1)
      }

    val (right, v) = go(ls, pos)
    (buf.toList ::: right, v)
  }

  // link : http://aperiodic.net/phil/scala/s-99/p20.scala
  def removeAt_1[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post) => (pre ::: post, e)
    case (pre, Nil) => throw new NoSuchElementException
  }

  // not tail recursion
  def removeAt_2[A](n: Int, ls: List[A]): (List[A], A) =
    if (n < 0) throw new NoSuchElementException
    else (n, ls) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, h :: tail) => (tail, h)
      case (_, h :: tail) =>
        val (t, e) = removeAt_2(n - 1, tail)
        (h :: t, e)
    }

}

object P21 {

  // P21 (*) Insert an element at a given position into a list
  def insertAt[A](v: A, n: Int, ls: List[A]): List[A] =
    ls.splitAt(n) match {
      case (left, right) => left ::: v :: right
    }
}

object P22 {

  // P22 (*) Create a list containing all integers within a given range
  def range(lo: Int, hi: Int): List[Int] = {
    @scala.annotation.tailrec
    def go(ls: List[Int], n: Int): List[Int] =
      if (n == lo) lo :: ls
      else go(n :: ls, n - 1)

    go(List(), hi)
  }

}

object P23 {

  import P20.removeAt

  def randomSelect[A](n: Int, ls: List[A]): List[A] = {

    def go(n: Int, ls: List[A], r: util.Random): List[A] =
      if (n <= 0) Nil
      else {
        val (rest, e) = removeAt(r.nextInt(ls.length), ls)
        e :: go(n - 1, rest, r)
      }

    go(n, ls, new util.Random)
  }
}