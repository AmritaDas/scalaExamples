package example

object higherorderlist {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
	//higher order list functions
	
	val nums = List(2, -4, 7, 5, 1)           //> nums  : List[Int] = List(2, -4, 7, 5, 1)
	
	val frt = List("def", "abc", "xyz", "mno", "ghi")
                                                  //> frt  : List[String] = List(def, abc, xyz, mno, ghi)
	
	nums map (_*2)                            //> res0: List[Int] = List(4, -8, 14, 10, 2)
	
	//creates a new collection
	nums.filter(_>3)                          //> res1: List[Int] = List(7, 5)
	
	//only restricts the domain of list
	nums.withFilter(_>3)                      //> res2: scala.collection.generic.FilterMonadic[Int,List[Int]] = scala.collecti
                                                  //| on.TraversableLike$WithFilter@4cb2c100
	
	nums partition (_>3)                      //> res3: (List[Int], List[Int]) = (List(7, 5),List(2, -4, 1))
	
	nums takeWhile (_>3)                      //> res4: List[Int] = List()
	
	nums takeWhile (_<3)                      //> res5: List[Int] = List(2, -4)
	
	frt span (_.charAt(0)<'g')                //> res6: (List[String], List[String]) = (List(def, abc),List(xyz, mno, ghi))
  
  //pack exercise
  
  def pack[T](xs:List[T]): List[List[T]] = xs match {
  	case Nil => Nil
  	case x::xs1 =>
  		val(first, rest) = xs span (y => y==x)
  		first::pack(rest)
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  
  val packer = List(3,3,3,4,5,5,6,7,7,6)          //> packer  : List[Int] = List(3, 3, 3, 4, 5, 5, 6, 7, 7, 6)
  
  pack(packer)                                    //> res7: List[List[Int]] = List(List(3, 3, 3), List(4), List(5, 5), List(6), Li
                                                  //| st(7, 7), List(6))
  //encode exercise
  
  def encode[T](xs:List[T]): List[(T, Int)] = {
  	pack(xs) map (ys => (ys.head,ys.length))
  }                                               //> encode: [T](xs: List[T])List[(T, Int)]
  
  encode(packer)                                  //> res8: List[(Int, Int)] = List((3,3), (4,1), (5,2), (6,1), (7,2), (6,1))
  
  
  
	//following operations exist for sequences including list, vector, range, array, string
	
	val xa = Vector(2,5,3)                    //> xa  : scala.collection.immutable.Vector[Int] = Vector(2, 5, 3)
	
	xa exists (_>3)                           //> res9: Boolean = true
	
	xa forall (_>3)                           //> res10: Boolean = false
	
	val pairs = List(1,4,7) zip xa            //> pairs  : List[(Int, Int)] = List((1,2), (4,5), (7,3))
	
	val (abc, ghi) = pairs.unzip              //> abc  : List[Int] = List(1, 4, 7)
                                                  //| ghi  : List[Int] = List(2, 5, 3)
	
	xa.sum                                    //> res11: Int = 10
	
	xa.product                                //> res12: Int = 30
	
	xa.max                                    //> res13: Int = 5
	
	xa.min                                    //> res14: Int = 2
	
	//a flatMap p = (a map p).flatten
	abc flatMap (x => ghi map (y => (x,y)))   //> res15: List[(Int, Int)] = List((1,2), (1,5), (1,3), (4,2), (4,5), (4,3), (7
                                                  //| ,2), (7,5), (7,3))
	
	(abc map (x => ghi map (y => (x,y)))).flatten
                                                  //> res16: List[(Int, Int)] = List((1,2), (1,5), (1,3), (4,2), (4,5), (4,3), (7
                                                  //| ,2), (7,5), (7,3))
	//scalar product
	def scalarProd (xs: Vector[Double], ys: Vector[Double]): Double =
		(xs zip ys).map(xy => xy._1 * xy._2).sum
                                                  //> scalarProd: (xs: Vector[Double], ys: Vector[Double])Double
		
	scalarProd((abc map (_.toDouble)).toVector, (ghi map (_.toDouble)).toVector)
                                                  //> res17: Double = 43.0
	



	case class Person(name:String, age:Int)
	
	val persons:Array[Person] = Array(new Person("abc", 20), new Person("ghi", 25), new Person("mno", 30))
                                                  //> persons  : Array[example.higherorderlist.Person] = Array(Person(abc,20), Pe
                                                  //| rson(ghi,25), Person(mno,30))
	
	//there's a difference between for loop and for expression
	//for loop
	for(p <- persons if p.age>23) p.name
	
	//for expression : this returns an array of p.names
	for(p <- persons if p.age>23) yield p.name//> res18: Array[String] = Array(ghi, mno)
	// res17: Array[String] = Array(ghi, mno)
	
	def scalarProdFor (xs: Vector[Double], ys: Vector[Double]): Double =
		(for((x,y) <- xs zip ys) yield x*y).sum
                                                  //> scalarProdFor: (xs: Vector[Double], ys: Vector[Double])Double
	
	scalarProdFor((abc map (_.toDouble)).toVector, (ghi map (_.toDouble)).toVector)
                                                  //> res19: Double = 43.0
	
	
	
  //reducing lists
  
  //sum(List(x1,x2,...xn)) = 0+x1+x2+...+xn
  //prod(List(x1,x2,...xn)) = 1*x1*x2*...*xn
  
  def sum (xs: List[Int]) = (0 :: xs) reduceLeft ((x,y) => (x + y))
                                                  //> sum: (xs: List[Int])Int
  
  def prod (xs: List[Int]) = (1 :: xs) reduceLeft ((x,y) => (x * y))
                                                  //> prod: (xs: List[Int])Int
  
  def sumR (xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
                                                  //> sumR: (xs: List[Int])Int
  
  def prodR (xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)
                                                  //> prodR: (xs: List[Int])Int
  
  //folding lists
  
  //foldLeft is like reduceLeft but takes an accumulator 'z' which is returned if the list is an empty list
  
  def sumF (xs: List[Int]) = (xs foldLeft 0) (_ + _)
                                                  //> sumF: (xs: List[Int])Int
  
  def prodF (xs: List[Int]) = (xs foldLeft 1) (_ * _)
                                                  //> prodF: (xs: List[Int])Int
  
  sum(nums)                                       //> res20: Int = 11
  prod(nums)                                      //> res21: Int = -280
  
  sumR(nums)                                      //> res22: Int = 11
  prodR(nums)                                     //> res23: Int = -280
  
	sumF(nums)                                //> res24: Int = 11
	prodF(nums)                               //> res25: Int = -280
	
	//concat
	
	def concat[T](xs:List[T], ys:List[T]): List[T] = (xs foldRight ys) (_ :: _)
                                                  //> concat: [T](xs: List[T], ys: List[T])List[T]
	
	//applications foldLeft and reduceLeft unfold on trees that lean to the left (work on individual elem)
	
	//applications foldRight and reduceRight unfold on trees that lean to the right (work on trees)
	
	//foldLeft and foldRight are equivalent ONLY for operators that are associative and commutative

}