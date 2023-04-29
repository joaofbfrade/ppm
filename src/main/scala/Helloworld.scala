object Helloworld extends App {



    def add (x:Int, y : Int) : Int = {

      return x+y;
    }


  def maior(x:Int,y:Int) = if (x > y) x else y
  def menor(x:Int,y:Int) = if (x > y) y else x

  def maiorde3(x:Int, y:Int, z:Int) = (maior((maior(x,y)), z))


  def middle3(x: Int, y: Int, z: Int) = (menor((maior(x, y)),
    maior(menor(x, y), z)))

  def largest(x: Int, y: Int, z: Int) = (maiorde3(x, y, z),
    middle3(x, y, z))


  def factorial (n:Int):Int ={
    if (n == 1)  1
    else n*factorial(n-1)
  }

  def pares (a:(Int,Int),b: (Int,Int)):(Int,Int) ={

    return (a._1+a._2,b._1*b._2)

  }

  def b3 (n:Int,x:Int,y:Int):(Int,Int)= (maior(x,maior(y,n)) , maior(x,n))

  println("pares: " + pares(  (1,1),(1,1)  )  )


}
