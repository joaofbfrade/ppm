object ficha4 extends App {


  def sum(xs: List[Int]) = (xs foldRight 0)(_ + _)


  def remDup[A](ls: List[A]): List[A] =
    (ls foldRight List[A]())((x, xs) => x :: (xs.dropWhile(_ == x)))


  type Team = String
  type Goals = Int
  type Match = ((Team, Goals), (Team, Goals))
  type Fixtures = List[Match]


  def same(m: Match): Boolean = m._1._1.equals((m._2._1))

  def draws(j: Fixtures): List[(Team, Team)] = {
    (j foldRight List[(Team, Team)]())((x, j) =>
      if (x._1._2 == x._2._2)
        ((x._1._1), (x._2._1)) :: j
      else
        j)

  }



    println(remDup(List(1,2,3)))

}