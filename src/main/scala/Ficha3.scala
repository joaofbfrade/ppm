import java.security.KeyStore.TrustedCertificateEntry

object Ficha3 extends App {

  import scala.annotation.tailrec


  def ex11a(x: Int): Int = {

    if (x == 0) 1 else
      x * ex11a(x - 1)
  }


  def ex11b(x: Int): Int = {

    x match {

      case 0 => 1
      case _ => x * ex11b(x - 1)
    }


  }


  def ex11c(n: Int): Int = {

    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0)
        acc
      else
        loop(acc * n, n - 1)

    loop(1, n)
  }


  def ex12a(lst: List[Char]): List[Char] = {

    lst match {

      case Nil => Nil
      case x :: y => x :: ex12a(lst.tail.dropWhile(_ == 'b'))


    }
  }


  def ex12b(lst: List[Int]): List[Int] = {

    def aux(lst1: List[Int], res: List[Int]): List[Int] = {

      lst1 match {
        case Nil => res
        case h :: t => aux(t.dropWhile(_ == h), res :+ h)
      }

    }

    aux(lst, Nil)
  }

  def ex3a(lst: List[Int], lst2: List[Int]): List[Int] = {


    (lst, lst2) match {

      case (_, Nil) => lst2
      case (Nil, _) => Nil
      case (h :: t, h2 :: t2) => h + h2 :: ex3a(t, t2)


    }
  }


  def islarger(x: Int, y: Int): Boolean = {
    if (x > y) true
    else false

  }


  def isSorted[A](lst: List[A], ordered: (A, A) => Boolean): Boolean = {


    lst match {


      case Nil => Nil
      case List(x) => true
      case (x :: y :: ys) => ordered(x, y) && isSorted(y :: ys, ordered)

    }


    def bubbleSort(data: List[Int], f: (Int, Int) => Boolean): List[Int] = {


      def aux(data: List[Int]): List[Int] = {
        data match {

          case Nil => Nil
          case x :: Nil => List(x)
          case x :: y :: xs => {

            if (f(x, y)) x +: aux(y :: xs)
            else
              y +: aux(x :: xs)


          }
        }

        def Untilsorted(lst: List[Int], sorted: Boolean): List[Int] = {


          if (sorted == true) lst

          else {
            val lst1 = aux(lst)
            Untilsorted(lst1, lst1 == lst)
          }
        }

        Untilsorted(data, false)
      }


    }


    def ex4a(lst: List[(Int, Int)]): List[(Int, Int)] = {

      lst match {
        case Nil => Nil
        case x :: y => {
          if (x._1 < x._2) x :: ex4a(y)

          else
            ex4a(y)

        }
      }


      def ex4b(lst: List[String]): String = {

        lst match {
          case Nil => ""
          case x :: xs => x ++ ex4b(xs)
        }
      }

    }


    def ex4c(lst: List[(Int, Int)]): List[Int] = {

      lst match {
        case Nil => Nil
        case x :: y => {
          if (x._1 > x._2) x._1 :: ex4c(y)

          else
            x._2 :: ex4c(y)

        }
      }


      def indicative(ind: String, telefs: List[String]): List[String] =
        telefs filter (x => x.substring(0, ind.length).equals(ind))

      println(bubbleSort(List(1, 7, 4, 3, 2), (x, y) => x < y))
    }


    def ex6(ind: String, telefs: List[String]): List[String] = {


      telefs match {

        case Nil => Nil
        case x :: y => {
          if (x.substring(0, ind.length).equals(ind))
            x :: ex6(ind, y)

          else ex6(ind, y)
        }

      }


    }
  }
  


}
