package example

object listasort {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val (rr,ss) = (3,4)                             //> rr  : Int = 3
                                                  //| ss  : Int = 4
	
	println(rr+ss)                            //> 7

  val p = ("jk",3)                                //> p  : (String, Int) = (jk,3)
  
  val q = (7,"jk",9)                              //> q  : (Int, String, Int) = (7,jk,9)
  
  //insertion sort
	def insSort (xs:List[Int]): List[Int] = xs match {
		case List() => List()
		case y :: ys => insert(y, insSort(ys))
	}                                         //> insSort: (xs: List[Int])List[Int]
	
	def insert (x:Int, xs:List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x<y) x :: xs else y :: insert(x,ys)
	}                                         //> insert: (x: Int, xs: List[Int])List[Int]
	
	//merge sort
	def mrgSort(xs:List[Int]): List[Int] = {
		val n = xs.length/2
		if(n == 0) xs
		else{
			def merge (xs:List[Int], ys:List[Int]):List[Int] = (xs,ys) match {
				case(Nil, ys) => ys
				case(xs, Nil) => xs
				case(x::xs1,y::ys1) =>
					if(x<y) x::merge(xs1,ys)
					else	y::merge(xs,ys1)
			}
			val (beg, end) = xs splitAt n
			merge(mrgSort(beg), mrgSort(end))
		}
	}                                         //> mrgSort: (xs: List[Int])List[Int]
	
	val nums = List(2, -4, 7, 5, 1)           //> nums  : List[Int] = List(2, -4, 7, 5, 1)
	
	insSort(nums)                             //> res0: List[Int] = List(-4, 1, 2, 5, 7)
	
	mrgSort(nums)                             //> res1: List[Int] = List(-4, 1, 2, 5, 7)
	
	//make merge sort applicable for all types
	//first, parameterize merge sort, i.e. T
	//second, add a comparison function
	
	def mrgSortPara[T](xs:List[T])(lt: (T,T) => Boolean): List[T] = {
		val n = xs.length/2
		if(n == 0) xs
		else{
			def merge (xs:List[T], ys:List[T]):List[T] = (xs,ys) match {
				case(Nil, ys) => ys
				case(xs, Nil) => xs
				case(x::xs1,y::ys1) =>
					if(lt(x,y)) x::merge(xs1,ys)
					else	y::merge(xs,ys1)
			}
			val (beg, end) = xs splitAt n
			merge(mrgSortPara(beg)(lt), mrgSortPara(end)(lt))
		}
	}                                         //> mrgSortPara: [T](xs: List[T])(lt: (T, T) => Boolean)List[T]
	
	mrgSortPara(nums)((x:Int, y:Int) => x < y)//> res2: List[Int] = List(-4, 1, 2, 5, 7)
	
	val frt = List("def", "abc", "xyz", "mno", "ghi")
                                                  //> frt  : List[String] = List(def, abc, xyz, mno, ghi)
	
	mrgSortPara(frt)((x:String, y:String) => x.compareTo(y)<0)
                                                  //> res3: List[String] = List(abc, def, ghi, mno, xyz)
	//or, use ordering
  def mrgSortOrd[T](xs: List[T])(ord: Ordering[T]): List[T] = {
		val n = xs.length/2
		if(n == 0) xs
		else{
			def merge (xs:List[T], ys:List[T]):List[T] = (xs,ys) match {
				case(Nil, ys) => ys
				case(xs, Nil) => xs
				case(x::xs1,y::ys1) =>
					if(ord.lt(x,y)) x::merge(xs1,ys)
					else	y::merge(xs,ys1)
			}
			val (beg, end) = xs splitAt n
			merge(mrgSortOrd(beg)(ord), mrgSortOrd(end)(ord))
		}
	}                                         //> mrgSortOrd: [T](xs: List[T])(ord: Ordering[T])List[T]
	
	mrgSortOrd(nums)(Ordering.Int)            //> res4: List[Int] = List(-4, 1, 2, 5, 7)
	
	mrgSortOrd(frt)(Ordering.String)          //> res5: List[String] = List(abc, def, ghi, mno, xyz)
	
	//or, use implicit ordering
  def mrgSortOrdImpl[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = xs.length/2
		if(n == 0) xs
		else{
			def merge (xs:List[T], ys:List[T]):List[T] = (xs,ys) match {
				case(Nil, ys) => ys
				case(xs, Nil) => xs
				case(x::xs1,y::ys1) =>
					if(ord.lt(x,y)) x::merge(xs1,ys)
					else	y::merge(xs,ys1)
			}
			val (beg, end) = xs splitAt n
			merge(mrgSortOrdImpl(beg), mrgSortOrdImpl(end))
		}
	}                                         //> mrgSortOrdImpl: [T](xs: List[T])(implicit ord: Ordering[T])List[T]
	
	mrgSortOrdImpl(nums)                      //> res6: List[Int] = List(-4, 1, 2, 5, 7)
	
	mrgSortOrdImpl(frt)                       //> res7: List[String] = List(abc, def, ghi, mno, xyz)
	
}