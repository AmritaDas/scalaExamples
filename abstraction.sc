package example
import secondweek.{Expr, Numb, Sum, Prod}
//sorted binary tree

abstract class intset {
	def incl(x:Int): intset
	def contains(x:Int): Boolean
	def union(other:intset):intset
}

/*could use singleton object for Empty instead
  object Empty extends intset{
		def contains(x:Int): Boolean = false
		def incl(x:Int): intset = new NonEmpty (x, new Empty, new Empty)
		override def toString = "."
  }
*/

class Empty extends intset{
	def contains(x:Int): Boolean = false
	def incl(x:Int): intset = new NonEmpty (x, new Empty, new Empty)
	def union(other:intset):intset = other
	override def toString = "."
}

class NonEmpty (elem:Int, left:intset, right:intset) extends intset {
	def contains(x:Int): Boolean = {
		if(x<elem) left contains x
		else if(x>elem) right contains x
		else true
	}
	
	def incl(x:Int): intset = {
		if(x<elem) new NonEmpty(elem, left incl x, right)
		else if (x>elem) new NonEmpty(elem, left, right incl x)
		else this
	}
	
	def union(other:intset):intset = ((left union right) union other) incl elem //???
	
	override def toString = "{" + left + elem + right + "}"

}

object abstraction {

	println("Welcome to the Scala worksheet") //> Welcome to the Scala worksheet
  
  val t1 = new NonEmpty(3, new Empty, new Empty)  //> t1  : example.NonEmpty = {.3.}

  val t2 = t1 incl 4                              //> t2  : example.intset = {.3{.4.}}
	
	//traits are like interfaces using 'with' keyword, (use extends if no abstract classes to extend). can define methods in traits. can't give parameters to traits like you can to classes
	
	//in Scala, the top class is Any (like Object in Java). Any has two subclasses - AnyVal (contains int/long/char/bool) and AnyRef (contains string/list/iterable/scalaobject).
	
	//one class, Nothing inherits from everything, and is returned when errors or exceptions are thrown to abnormally terminate the program.
	//and one class, Null inherits from everything in AnyRef. So, String, List, Object, etc. can be assigned null values.
	//But int/long/char cannot be assigned null values. This is unlike Java, which had int (couldn't accept null) and Integer (could accept null)
	
	
	//type bounds:
	//allows only subtypes
	def assertAllPos[S <: NonEmpty](r:S): S = ???
                                                  //> assertAllPos: [S <: example.NonEmpty](r: S)S
	//allows only supertypes
	def assertAllPosS[S >: intset](r:S): S = ???
                                                  //> assertAllPosS: [S >: example.intset](r: S)S
	//mixed bounds: S > Empty, S < intset
	def assertAllPosM[S >: Empty <: intset](r:S): S = ???
                                                  //> assertAllPosM: [S >: example.Empty <: example.intset](r: S)S
	//subtyping like this allowed
	val c:intset = new Empty                  //> c  : example.intset = .
	val gi:Array[Empty] = Array(new Empty)    //> gi  : Array[example.Empty] = Array(.)
	//but subtyping in arrays doesn't work
	//val cg:Array[intset] = gi
	
	
	
	//variance - invariant, covariant, contravariant
	//http://docs.scala-lang.org/tutorials/tour/variances.html
	
	//covariant
	class A {}
		
	class B[+T] extends A{}
	
	//B is subtype of A, List[B] is subtype of List[A], function that accepts A will accept B
	
	//contravariant
	class C {}
		
	class D[-T] extends C{}
	
	//D is subtype of C, List[C] is subtype of List[D], function that accepts D will accept C
	
	//invariant
	class E{}
	
	class F[T] extends E{}
	
	//F is subtype of E, but List[E] is not subtype of List[F] or other way around, function that accepts E will not accept F or other way around
	
	
	
	//recursive methods need result type
	//use match for pattern matching
	
	def show(e:Expr): String = e match {
		case Numb(x) => x.toString
		case Sum(l,r) => "( " + show(l) + " + " + show(r) + " )"
		case Prod(o,p) => show(o) + " * " + show(p)
	}                                         //> show: (e: example.secondweek.Expr)String
	
	show(Sum(Numb(2), Numb(5)))               //> res0: String = ( 2 + 5 )
	
	show(Sum(Prod(Numb(2),Numb(3)), Numb(5))) //> res1: String = ( 2 * 3 + 5 )

	show(Prod(Sum(Numb(2),Numb(3)), Numb(5))) //> res2: String = ( 2 + 3 ) * 5

	show(Prod(Sum(Numb(2),Numb(3)), Sum(Numb(4),Numb(5))))
                                                  //> res3: String = ( 2 + 3 ) * ( 4 + 5 )
	
}