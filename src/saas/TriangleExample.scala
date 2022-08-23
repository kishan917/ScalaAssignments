package saas

import scala.util.{Failure, Success, Try}

object TriangleExample extends App{

  val triSideList = List((3,4,5), (3,4,4), (3,3,3), (3,2,1))
//  val triSideList = List((3,4,5), (3,4,4), (3,3,3), (3,2,1)).reverse

  triSideList.foreach(t => {
    val (a,b,c) = t
    Try(new Equilateral(a, b, c))
      .orElse(Try(new RightAngled(a, b, c)))
      .orElse(Try(new Isosceles(a, b, c)))
      .orElse(Try(new Triangle(a, b, c)))
    match {
      case Success(tri) => println(tri.triangleType)
      case Failure(exception) => println("invalid")
    }
  })

}


case class TriangleException(msg: String) extends Exception(msg: String)

class Triangle(a: Int, b: Int, c: Int) {
  val triangleType: String = "notspecial"
  def isTriangle: Boolean = {
    if( (a+b-c)>0 && (b+c-a)>0 && (a+c-b)>0 ) true
    else false
  }
  override def toString: String = s"Triangle<$a, $b, $c>"
  if(!isTriangle) throw TriangleException(s"Can not create Triangle with sides: $a, $b, $c")
}

class RightAngled(a: Int, b: Int, c: Int) extends Triangle(a:Int, b:Int, c:Int) {
  override val triangleType: String = "right-angled"
  def isRightAngled: Boolean = {
    if(
      super.isTriangle
        && ( (BigInt(a).pow(2) + BigInt(b).pow(2) == BigInt(c).pow(2))
            || (BigInt(b).pow(2) + BigInt(c).pow(2) == BigInt(a).pow(2))
            || (BigInt(a).pow(2) + BigInt(c).pow(2) == BigInt(b).pow(2))
        )
    ) true
    else false
  }
  if(!isRightAngled) throw TriangleException(s"Can not create RightAngled Triangle with sides: $a, $b, $c")
}

class Isosceles(a: Int, b: Int, c: Int) extends Triangle(a:Int, b:Int, c:Int) {
  override val triangleType: String = "isosceles"
  def isIsosceles: Boolean = {
    if(super.isTriangle && ( a==b || b==c || a==c )) true
    else false
  }
  if(!isIsosceles) throw TriangleException(s"Can not create Isosceles Triangle with sides: $a, $b, $c")
}

class Equilateral(a: Int, b: Int, c: Int) extends Triangle(a:Int, b:Int, c:Int) {
  override val triangleType: String = "equilateral"
  def isEquilateral: Boolean = {
    if(super.isTriangle && ( a==b && b==c )) true
    else false
  }
  if(!isEquilateral) throw TriangleException(s"Can not create Equilateral Triangle with sides: $a, $b, $c")
}

