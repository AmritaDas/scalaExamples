package example

object nqueens {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def queens(n:Int): Set[List[Int]] = {
  	def placeQueens(k:Int): Set[List[Int]] = {
  		if(k==0) Set(List())
  		else{
  			for{
  				queens <- placeQueens(k-1)
  				col <- 0 until n
  				if(isSafe(col, queens))
  			}
  			yield col :: queens
  		}
  	}
  	placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  def isSafe(col:Int, queens:List[Int]): Boolean = {
  	val row = queens.length
  	val queensWithRow = (row-1 to 0 by -1) zip queens
  	queensWithRow forall{
  		case (r,c) => col != c && math.abs(col-c) != row-r
  	}
  }                                               //> isSafe: (col: Int, queens: List[Int])Boolean
  
  def show(queens: List[Int]) = {
  	val lines = for(col <- queens.reverse)
  									yield Vector.fill(queens.length)("x ").updated(col, "Q ").mkString
  	"\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
  
  (queens(4) map show) mkString "\n"              //> res0: String = "
                                                  //| x x Q x 
                                                  //| Q x x x 
                                                  //| x x x Q 
                                                  //| x Q x x 
                                                  //| 
                                                  //| x Q x x 
                                                  //| x x x Q 
                                                  //| Q x x x 
                                                  //| x x Q x "
  
  
  
  val frt = List("apple", "pear", "pineapple", "orange", "mango")
                                                  //> frt  : List[String] = List(apple, pear, pineapple, orange, mango)
  
  //sort by length
  frt sortWith(_.length < _.length)               //> res1: List[String] = List(pear, apple, mango, orange, pineapple)
  
  //sort asc
  frt.sorted                                      //> res2: List[String] = List(apple, mango, orange, pear, pineapple)
  
  frt.groupBy(_.head)                             //> res3: scala.collection.immutable.Map[Char,List[String]] = Map(m -> List(man
                                                  //| go), p -> List(pear, pineapple), a -> List(apple), o -> List(orange))
	
	
	
	//polynomial example
	class Poly(val terms: Map[Int, Double]) {
		def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
		
		def adjust(term:(Int,Double)): (Int,Double) = {
			val (exp, coeff) = term
			terms get exp match {
				case Some(coeff1)=> exp -> (coeff + coeff1)
				case None=> exp -> coeff
			}
		}
		
		override def toString = {
			(	for ((exp,coeff) <- terms.toList.sorted.reverse)
					yield coeff + "x^" + exp
			) mkString " + "
		}
	}
	
	//6x^5 + 4x^3 + 2x
	val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.0))
                                                  //> p1  : example.nqueens.Poly = 6.0x^5 + 4.0x^3 + 2.0x^1
	
	//7x^3 + 3
	val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))//> p2  : example.nqueens.Poly = 7.0x^3 + 3.0x^0
	
	//6x^5 + 11x^3 + 2x + 3
	p1 + p2                                   //> res4: example.nqueens.Poly = 6.0x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0



	//simpler polynomial example
	class PolyS(term0: Map[Int, Double]) {			//change in argument here
	
		def this(binding: (Int, Double)* ) = this(binding.toMap)
		
		val terms = term0 withDefaultValue 0.0
		
		def + (other: PolyS) = new PolyS(terms ++ (other.terms map adjust))
		
		def adjust(term:(Int,Double)): (Int,Double) = {
			val (exp, coeff) = term
			exp -> (coeff + terms(exp))
		}
		
		override def toString = {
			(	for ((exp,coeff) <- terms.toList.sorted.reverse)
					yield coeff + "x^" + exp
			) mkString " + "
		}
	}
	
	//pass directly instead of map
	val p3 = new PolyS(1 -> 2.0, 3 -> 4.0, 5 -> 6.0)
                                                  //> p3  : example.nqueens.PolyS = 6.0x^5 + 4.0x^3 + 2.0x^1
  
  //or with map
  val p4 = new PolyS(Map(0 -> 3.0, 3 -> 7.0))     //> p4  : example.nqueens.PolyS = 7.0x^3 + 3.0x^0
  
  p3 + p4                                         //> res5: example.nqueens.PolyS = 6.0x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0


	
	//polynomial example with foldleft <- this is more efficient
	class PolyF(term0: Map[Int, Double]) {
	
		def this(binding: (Int, Double)* ) = this(binding.toMap)
		
		val terms = term0 withDefaultValue 0.0
		
		def + (other: PolyF) = new PolyF((other.terms foldLeft terms)(addTerm))
		
		def addTerm(terms: Map[Int,Double], term: (Int, Double)): Map[Int,Double] = {
			val (exp, coeff) = term
			terms + (exp -> (coeff + terms(exp)))
		}
		
		override def toString = {
			(	for ((exp,coeff) <- terms.toList.sorted.reverse)
					yield coeff + "x^" + exp
			) mkString " + "
		}
	}
	
	val p5 = new PolyF(1 -> 2.0, 3 -> 4.0, 5 -> 6.0)
                                                  //> p5  : example.nqueens.PolyF = 6.0x^5 + 4.0x^3 + 2.0x^1
	
	val p6 = new PolyF(Map(0 -> 3.0, 3 -> 7.0))
                                                  //> p6  : example.nqueens.PolyF = 7.0x^3 + 3.0x^0
	
	p5 + p6                                   //> res6: example.nqueens.PolyF = 6.0x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
	
}