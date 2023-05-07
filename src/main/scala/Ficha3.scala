object Ficha3 extends App{
  import scala.annotation.tailrec


  def ex11a (x:Int):Int = {

    if(x==0) 1 else
    x*ex11a (x-1)
  }



  def ex11b (x:Int):Int = {

    x match {

      case 0 => 1
      case _ => x*ex11b(x-1)
    }


  }


  def ex11c(n:Int):Int = {

    @tailrec
    def loop(acc: Int, n: Int): Int =
      if (n == 0)
        acc
      else
        loop(acc * n, n - 1)

    loop(1, n)
  }


  def ex12a(lst:List[Char]):List[Char] = {

    lst match {

      case Nil => Nil
      case x::y => {

        val b = b :: x
        

        if ()
    }
    }


  }

  println(ex11a(3))

}
