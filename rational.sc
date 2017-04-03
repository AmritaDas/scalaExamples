package example

object rational {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val x = new Rationa(1,2)                        //> x  : example.Rationa = 1/2
  val y = new Rationa(2,3)                        //> y  : example.Rationa = 2/3
  val z = new Rationa(3,4)                        //> z  : example.Rationa = 3/4
  
  x.+(y)                                          //> res0: example.Rationa = 7/6
  
  x.-(y)                                          //> res1: example.Rationa = -1/6
  
  x.max(y)                                        //> res2: example.Rationa = 2/3

	//infix notation
	x + y                                     //> res3: example.Rationa = 7/6
	
	x - y                                     //> res4: example.Rationa = -1/6
	
	x < y                                     //> res5: Boolean = true
	
	x + y + z                                 //> res6: example.Rationa = 23/12
	
	x + y - z                                 //> res7: example.Rationa = 5/12
	
	-x                                        //> res8: example.Rationa = -1/2

	//precedence determined properly on its own through scala's compiler
	x * x + y * y                             //> res9: example.Rationa = 25/36
}

class Rationa(x:Int, y:Int) {  //primary constructor
  require(y != 0, "denominator must be non zero")    //requirement
  //or
  assert(y != 0)
  
  def this(x:Int) = this(x,1)  //auxiliary constructor
  
  private def gcd(a:Int, b:Int): Int = if(b==0) a  else gcd(b, a%b)
  
  private val g = if (gcd(x,y)<0) -gcd(x,y) else gcd(x,y)
  
  def numer = x/g
  def denom = y/g
  
  def < (that:Rationa) = numer * that.denom < denom * that.numer
  
  def max(that:Rationa) = if(this < that) that else this
  
  def + (that:Rationa) = new Rationa (numer * that.denom + denom * that.numer, denom * that.denom)
  
  def unary_- : Rationa = new Rationa(-numer, denom)
  
  def - (that:Rationa) = this + -that
  
  def * (that:Rationa) = new Rationa (numer * that.numer, denom * that.denom)
  
  override def toString = numer + "/" + denom

}