package example

object andornotxs {

/*
call by name: lazy evaluation  <- recomputes whenever called. def keyword evaluates by name
call by value: eager evaluation <- computes once at point of definition. in practice, more efficient. val keyword evaluates by value

if cbv terminates, cbn will terminate. 
other way around is not true. this is cos of infinite loop condition. If y below was an infinite loop, cbv would not terminate, but cbn would

cbv by default, everything is cbv here
def con (x:Int, y:Int) : Int = x

cbn can be forced like below. y will be evaluated as cbn
def con (x:Int, y: => Int) : Int = x
*/

  //y should be call by name. because in case y is infinite loop, there isn't chance of function failing if y is call by name 
  def and(x: Boolean, y: => Boolean): Boolean = {
  	if(x) y 
  	else false
  }
  
  def or(x: Boolean, y: Boolean): Boolean = {
  	if(x) true 
  	else y
  }
  
  def not(x: Boolean): Boolean = {
    if(x) false
    else true
  }
  
  def xor(x: Boolean, y: Boolean): Boolean = {
    if(x){
      if(y) false
      else true
    }
    else{
      if(y) true
      else false
    }
  }
  
  def sqrt(x: Double): Double = {
    sqrtest(x, 1.0)
  }
  
  def sqrtest(x: Double, est: Double): Double = {
    val y:Double = x/est
    val mean:Double = (est+y)/2
    if((mean*mean - x)/x < 0.001) (math rint mean*1000)/1000
    else sqrtest(x, mean)
  }
  
  //recursive factorial
  def fact(x:Int): Long = {
    if(x == 0) 1
    else x * fact(x-1)
  }
  
  //tail recursive factorial
  def tailfact(n:Int): Long = {
    def loop(acc:Long, n:Long): Long = {
      if(n==0) acc
      else loop(acc * n, n-1)
    }
    loop(1, n)
  }

}