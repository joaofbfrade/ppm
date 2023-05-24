object ficha5 extends App{

  sealed trait MyTree[+A]

  case object Empty extends MyTree[Nothing]

  case class Node[A](value: A, left: MyTree[A], right: MyTree[A]) extends MyTree[A]

  case class Example[A](myField: MyTree[A]) {
    def maximum() = Example.maximum(this.myField.asInstanceOf[MyTree[Int]])

    def depth() = Example.depth(this.myField)

    def map[B](f: A => B): MyTree[B] = Example.map(this.myField)(f)
  }

  object Example {
    def maximum(t: MyTree[Int]): Option[Int] = {

        t match {
          case Empty=>None
          case Node(value,left,right)=>{

            
          }

        }
    }

    def depth[A](t: MyTree[A]): Int = {
      ???
    }

    def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = {
      ???
    }

    def main(args: Array[String]): Unit = {
      val tree1 = Node(42, Node(8, Empty, Empty), Empty)
      val t = Example(tree1)
      println(s"Maximum element of the tree: ${t.maximum()}")
      println(s"Depth of the tree: ${t.depth()}")
      println(s"Map: ${t.map((x: Int) => x * 2)}")
    }
  }


}
