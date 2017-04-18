package example

object secondweek {

  /*Functions that take other functions as parameters, or that return functions as results are called higher order functions. 
   * A first order function acts on data types, simple data types such as ints or longs or lists, but not other functions. 
   * Whereas, a higher order function acts on other functions.
   */
  
  def id(x:Int): Int = x
  def cube(x:Int): Int = x*x*x
  def fact(x:Int): Int = if(x==0) 1 else x*fact(x-1)
  
  //get sum of all numbers between a and b
  def sumInts(a:Int, b:Int): Int = sum(id, a, b)
  //eg: sumInts(3,5) = 12
  
  //get sum of cubes of all numbers between a and b
  def sumCubes(a:Int, b:Int): Int = sum(cube, a, b)
  //eg: sumCubes(3,5) = 216
  
  //get sum of factorials of all numbers between a and b
  def sumFact(a:Int, b:Int): Int = sum(fact, a, b)
  
  //where sum is a higher order function defined as follows:
  //here, f is any generic function passed as a parameter
  def sum(f:Int => Int, a:Int, b:Int): Int = {
    if(a>b) 0
    else f(a) + sum(f, a+1, b) 
  }
  
  
  /* Multiple Parameter Lists
   * function application is left associative ??
   * sumMP(cube)(3,5) = (sumMP(cube))(3,5) ??
   * 
   * functional return types are right associative
   * def f(arg1)(arg2)...(arg n) => E 
   * is equivalent to
   * def f = (arg1 => (arg2 => ... (arg n => E)...))
   */
  def sumMP(f:Int => Int)(a:Int, b:Int) : Int = {
    if(a>b) 0
    else f(a) + sumMP(f)(a+1, b)
  }
  //eg: sumMP(cube)(3,5) = 216
  //or: sumMP(x=>x)(3,5) = 12

  
  /* Anonymous functions. 
   * A string does't have to be named to use it. Like this: println("abc")
   * Similar thing can be done with functions in Scala
   */
  
  //cube
  (x:Int) => x*x*x
  
  //sum
  (x:Int, y:Int) => x+y
  
  //get sum of all numbers between a and b
  def sumIntsAnon(a:Int, b:Int): Int = sum(x => x, a, b)
  //eg: sumIntsAnon(3,5) = 12
  
  //get sum of cubes of all numbers between a and b
  def sumCubesAnon(a:Int, b:Int): Int = sum(x => x*x*x, a, b)
  //eg: sumCubesAnon(3,5) = 216
  
  //tail recursive higher order function 
  def sumEx(f: Int => Int)(a:Int, b:Int): Int = {
    def loop (a: Int, acc: Int): Int = {
      if(a>b) acc
      else loop(a+1, f(a) + acc)
    }
    loop(a,0)
  }
  
  
  /* Functions returning functions
   */
  def sumRet(f:Int => Int): (Int, Int) => Int = {
    def sumF(a:Int, b:Int): Int = {
      if(a>b) 0
      else f(a) + sumF(a+1, b)
    }
    sumF
  }
  //eg: sumRet(cube)(3,5) = 216
  //or: sumRet(x=>x)(3,5) = 12
  
  //get sum of all numbers between a and b
  def sumIntsRet = sumRet(x => x)
  //eg: sumIntsRet(3,5) = 12
  
  //get sum of cubes of all numbers between a and b
  def sumCubesRet = sumRet(x => x*x*x)              //!!! IMP
  //eg: sumCubesRet(3,5) = 216


  def product(f:Int => Int): (Int, Int) => Int = {
    def prod(a:Int, b:Int): Int = {
      if (a>b) 1
      else f(a) * prod(a+1,b)
    }
    prod
  }
  //eg: product(x=>x)(3,5) = 60
   
  def prodMP(f:Int => Int)(a:Int, b:Int): Int = {
    if(a>b) 1
    else f(a) * prodMP(f)(a+1, b)
  }
  //eg: prodMP(x=>x)(3,5) = 60
  
  def factprod(n:Int): Int = product(x=>x)(1,n)
  //eg: factprod(5) = 120
  
  //generalize sum and product
  def gen(f: Int => Int, combine: (Int, Int) => Int, ret: Int)(a: Int, b: Int): Int = {
    if(a>b) ret
    else combine(f(a), gen(f, combine, ret)(a+1,b))
  }
  
  def prodgen(f:Int => Int)(a: Int, b: Int): Int = gen(f, (x,y) => x*y, 1)(a,b)
  
  def sumgen(f:Int => Int)(a: Int, b: Int): Int = gen(f, (x,y) => x+y, 0)(a,b)
  
  
  
  //
  trait Expr
  
  case class Numb(n:Int) extends Expr
  
  case class Sum(e1:Expr, e2:Expr) extends Expr
  
  case class Prod(e1:Expr, e2:Expr) extends Expr

}

