object ficha2  extends App {


  def empty[A](lst: List[A]) = lst.length == 0

  def length(lst: List[Int]): Int = {
    lst match {
      case Nil => 0
      case _ :: tail => 1 + length(tail)
    }
  }


  def distance(lstPoint: List[(Double, Double)]): List[Double] = {
    lstPoint match {
      case Nil => Nil;
      case (x, y) :: tail => (Math.sqrt(x * x + y * y)) ::
        distance(tail);
    }
  }

  //

  def ex1a(lst: List[Int]) : List[Int] = {


    lst match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: y :: tail => y :: x :: ex1a(tail)
    }
  }

  def ex1b (lst:List[Int]):Int= {

    lst match {
      case Nil => 1
      case head :: tail => head * ex1b(tail)
    }
  }


def ex1c (lst:List[Int],x:Int):List[Int]= {

  lst match {

    case Nil => Nil
    case y :: Nil => y :: List(x)
    case y => y ++ List(x)
  }
}

  def ex1d[E](lst1: List[E], lst2: List[E]): List[E] = {
    lst1 match {
      case List() => lst2
      case h :: t => h :: ex1d(t, lst2)
    }
  }

  def ex1fa (lst:List[Int]):(Int,Int)=
    {
      lst match {

        case Nil=>(0,0)
        case x::s => {
          val y = y+1

           x+ex1fa(s)


        }

      }



    }



  println(  ex1fa(  List(1,2,3,4)))
}
