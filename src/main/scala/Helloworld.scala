object Helloworld extends App {

// 2 A
  def ex2a(a: (Int, Int), b: (Int, Int)): (Int, Int) = {

    return (a._1 + a._2, b._1 * b._2)

  }

// 2b
  def maior(x:Int,y:Int) = if (x > y) x else y
  def menor(x:Int,y:Int) = if (x > y) y else x

  def maiorde3(x:Int, y:Int, z:Int) = (maior((maior(x,y)), z))


  def middle3(x: Int, y: Int, z: Int) = (menor((maior(x, y)),
    maior(menor(x, y), z)))

  def ex2b(x: Int, y: Int, z: Int) = (maiorde3(x, y, z),
    middle3(x, y, z))




  // 2C


  def ex2c(x:Int,y:Int,z:Int): (Int,Int,Int)  = ( maior(maior(x,y),z)   ,middle3(x,y,z)   ,  menor(menor(x,y),z)  )

  // 2D

  def ex2d(x:Int,y:Int,z:Int): (Boolean) =  x + y > z && x +z > y && y + z > x



  // 2E

  def ex2e (str: String) = {
    val lst: Array[String] = str.split(" ");
    lst.head ++ " " ++
      lst.last
  }


  def ex2e2(str: String) = {
   val nome = str.split(" ")
    nome.head + "  " + nome.last
  }
  //3a


  def ex3a (x:Int,y:Int): Int =
   if(y==0) 1 else x*(ex3a(x, y - 1))



  //3B

     def ex3b(lst:List[Int]) = (lst.head,lst.last)

  //3C

  def ex3c(lst:List[Int]) = (lst, lst.size)
  //3D


  def ex3d (lst:List[Double]):Double = if (lst.isEmpty) 0 else (lst.head+ex3d(lst.tail))/lst.length



  print( ex3d (List(1.3,2.2,3.1) ))

//nice

}
