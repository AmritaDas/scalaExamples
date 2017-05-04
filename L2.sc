package example

object L2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val a = List(1,2,3,4,5)                         //> a  : List[Int] = List(1, 2, 3, 4, 5)
  
  val b = List(2,4,6,8,10)                        //> b  : List[Int] = List(2, 4, 6, 8, 10)

	//this doesn't work if we use list range like 1 to 5
  for((x,y) <- a zip b)
  	yield(x+y)                                //> res0: List[Int] = List(3, 6, 9, 12, 15)

	val c = 1 to 5                            //> c  : scala.collection.immutable.Range.Inclusive = Range(1, 2, 3, 4, 5)
  
  val d = 2 to 10 by 2                            //> d  : scala.collection.immutable.Range = Range(2, 4, 6, 8, 10)
   
  //this also works but without range
  for((x,y) <- c zip d)
  	yield(x+y)                                //> res1: scala.collection.immutable.IndexedSeq[Int] = Vector(3, 6, 9, 12, 15)

	((1 to 5) zip (2 to 10 by 2)).foreach(x => println(x._1 + x._2))
                                                  //> 3
                                                  //| 6
                                                  //| 9
                                                  //| 12
                                                  //| 15

	def isPrime(n: Int): Boolean = (2 until n) forall (n%_ != 0)
                                                  //> isPrime: (n: Int)Boolean

	isPrime(67)                               //> res2: Boolean = true
	
	//find second prime number between 1000 and 10000
	//finds all primes first, so very inefficient
	((1000 to 10000) filter isPrime) (1)      //> res3: Int = 1013
	
  //can be made efficient if we avoid computing the tail of the result until it is needed for evaluation
  //streams are like lists, but their tail is evaluated only on demand
  
  //cons is call by name
  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
                                                  //> xs  : Stream.Cons[Int] = Stream(1, ?)
  
  Stream(1,2,3)                                   //> res4: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  
  (1 to 1000).toStream                            //> res5: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  
  //Stream(1, ?) <- tail not yet evaluated
  
  ((1000 to 10000).toStream filter isPrime) (1)   //> res6: Int = 1013
  
  def listRange(lo:Int, hi:Int): List[Int] = {
  	if(lo >= hi) Nil
  	else lo :: listRange(lo+1, hi)
  }                                               //> listRange: (lo: Int, hi: Int)List[Int]
  
  def streamRange(lo:Int, hi:Int): Stream[Int] = {
  	if(lo >= hi) Stream.empty
  	else Stream.cons(lo, streamRange(lo+1, hi))
  	//or use: else lo #:: streamRange(lo+1, hi)
  }                                               //> streamRange: (lo: Int, hi: Int)Stream[Int]
  
  //this evaluated whole range
  Stream(2 to 10 by 2)                            //> res7: scala.collection.immutable.Stream[scala.collection.immutable.Range] =
                                                  //|  Stream(Range(2, 4, 6, 8, 10), ?)
  //instead do
  //this also evaluates whole range then converts to stream
  //but when doing big operations on the Stream, this is useful since you perform the big operation only as far as the stream progresses
  (2 to 10 by 2).toStream                         //> res8: scala.collection.immutable.Stream[Int] = Stream(2, ?)
  
  //IMP: however, another inefficiency is whenever tail is called, the whole stream is recomputed every time
  //i.e. for 1001, 1000 is recomputed, 1001 is computed
  //then for 10002, 1000 is recomputed, 1001 is recomputed, 1002 is computed
  //this can be avoided by storing the result of the first evaluation of the tail and reusing it instead of recomputing tail
  //this is called lazy evaluation
  
  val y1 = 1000 to 10000                          //> y1  : scala.collection.immutable.Range.Inclusive = Range(1000, 1001, 1002, 
                                                  //| 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010, 1011, 1012, 1013, 1014, 101
                                                  //| 5, 1016, 1017, 1018, 1019, 1020, 1021, 1022, 1023, 1024, 1025, 1026, 1027, 
                                                  //| 1028, 1029, 1030, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 104
                                                  //| 0, 1041, 1042, 1043, 1044, 1045, 1046, 1047, 1048, 1049, 1050, 1051, 1052, 
                                                  //| 1053, 1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061, 1062, 1063, 1064, 106
                                                  //| 5, 1066, 1067, 1068, 1069, 1070, 1071, 1072, 1073, 1074, 1075, 1076, 1077, 
                                                  //| 1078, 1079, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 109
                                                  //| 0, 1091, 1092, 1093, 1094, 1095, 1096, 1097, 1098, 1099, 1100, 1101, 1102, 
                                                  //| 1103, 1104, 1105, 1106, 1107, 1108, 1109, 1110, 1111, 1112, 1113, 1114, 111
                                                  //| 5, 1116, 1117, 1118, 1119, 1120, 1121, 1122, 1123, 1124, 1125, 1126, 1127, 
                                                  //| 1128, 1129, 1130, 1131, 1132, 1133, 1134, 1135, 1136, 1137, 1138, 1139, 114
                                                  //| 0, 1141, 1142, 1143, 11
                                                  //| Output exceeds cutoff limit.
  
  def z1 = 1000 to 10000                          //> z1: => scala.collection.immutable.Range.Inclusive
  
  lazy val x1 = 1000 to 10000                     //> x1: => scala.collection.immutable.Range.Inclusive
  
  //difference between def and lazy val is that def recomputes every time, while lazy val can reuse stored value
  
  def expr = {
  	val x = {print("x"); 1} 			//evaluated immediately
  	lazy val y = {print("y"); 1}	//evaluated when called, then stored, stored value used next time
  	def z = {print("z"); 1}			//evaluated when called, recomputed every time
  	z+y+x+z+y+x
  }                                               //> expr: => Int
  //the 1 at the end is just to make it unit
  
  expr                                            //> xzyzres9: Int = 6
  //this prints 'xzyz' like this:
  //x is printed as soon as it executes val line
  //y is not printed at lazy val line
  //z is not printed at def line
  //z is printed at 1st member of z+y+x+z+y+x since not evaluated before
  //y is printed since not evaluated before
  //x is not printed since already evaluated
  //z is printed since def recomputes every time
  //y is not printed since already evaluated
  //x is not printed since already evaluated
  //6 is printed since each function is executed every time, but evaluated differently
  
  
  (streamRange(1000, 10000) filter isPrime) apply 1
                                                  //> res10: Int = 1013
  //apply is defined like this:
  //def apply(n:Int): T =
  //	if(n==0) head
  //	else tail.apply(n-1)
  
  //so, after the 1st prime, 1009 apply 1, it becomes apply 0,
  //so, at 1013 apply 0, the head i.e. 1013 is returned
  
  
  //a better way to check primes is: if %x is not 0, then all multiples of x do not have to be checked
  //called Sieve of Eratosthenes
  
  def from(n:Int): Stream[Int] = n #:: from(n+1)  //> from: (n: Int)Stream[Int]
  
  //filter to those which are not multiples
  def sieve(s:Stream[Int]): Stream[Int] =
  	s.head #:: sieve(s.tail filter (_ % s.head != 0))
                                                  //> sieve: (s: Stream[Int])Stream[Int]
  
  val primes = sieve(from(2))                     //> primes  : Stream[Int] = Stream(2, ?)
  
  //toList forces it to evaluate
  primes.take(100).toList                         //> res11: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
                                                  //|  47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 
                                                  //| 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 
                                                  //| 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 
                                                  //| 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 
                                                  //| 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 
                                                  //| 479, 487, 491, 499, 503, 509, 521, 523, 541)
  
  	
	//patmat
	def times(chars: List[Char]): List[(Char, Int)]	= {
	  def count(charMap: scala.collection.mutable.Map[Char, Int], c: Char) = {
	  	val f = charMap.get(c).getOrElse(0)
	  	charMap.put(c, f+1)
	  	charMap
	  }
	  
	  var cMap = scala.collection.mutable.Map[Char, Int]()
	  for(c <- chars){
	  	cMap = count(cMap, c)
	  }
	  
	  cMap.toList
	}                                         //> times: (chars: List[Char])List[(Char, Int)]
  
  times(List('a','b','a'))                        //> res8: List[(Char, Int)] = List((b,1), (a,2))
  
}