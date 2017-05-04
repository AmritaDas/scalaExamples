package example

import scala.io.Source

object telephoneletters {

  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
                                                  //> in  : scala.io.BufferedSource = non-empty iterator
	
	val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
                                                  //> words  : List[String] = List(Aarhus, Aaron, Ababa, aback, abaft, abandon, ab
                                                  //| andoned, abandoning, abandonment, abandons, abase, abased, abasement, abasem
                                                  //| ents, abases, abash, abashed, abashes, abashing, abasing, abate, abated, aba
                                                  //| tement, abatements, abater, abates, abating, Abba, abbe, abbey, abbeys, abbo
                                                  //| t, abbots, Abbott, abbreviate, abbreviated, abbreviates, abbreviating, abbre
                                                  //| viation, abbreviations, Abby, abdomen, abdomens, abdominal, abduct, abducted
                                                  //| , abduction, abductions, abductor, abductors, abducts, Abe, abed, Abel, Abel
                                                  //| ian, Abelson, Aberdeen, Abernathy, aberrant, aberration, aberrations, abet, 
                                                  //| abets, abetted, abetter, abetting, abeyance, abhor, abhorred, abhorrent, abh
                                                  //| orrer, abhorring, abhors, abide, abided, abides, abiding, Abidjan, Abigail, 
                                                  //| Abilene, abilities, ability, abject, abjection, abjections, abjectly, abject
                                                  //| ness, abjure, abjured, abjures, abjuring, ablate, ablated, ablates, ablating
                                                  //| , ablation, ablative, ab
                                                  //| Output exceeds cutoff limit.
	
	val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
                                                  //> mnem  : scala.collection.immutable.Map[Char,String] = Map(8 -> TUV, 4 -> GHI
                                                  //| , 9 -> WXYZ, 5 -> JKL, 6 -> MNO, 2 -> ABC, 7 -> PQRS, 3 -> DEF)
	
	val charCode: Map[Char, Char] = for((digit, str) <- mnem; ltr <- str) yield ltr -> digit
                                                  //> charCode  : Map[Char,Char] = Map(E -> 3, X -> 9, N -> 6, T -> 8, Y -> 9, J -
                                                  //| > 5, U -> 8, F -> 3, A -> 2, M -> 6, I -> 4, G -> 4, V -> 8, Q -> 7, L -> 5,
                                                  //|  B -> 2, P -> 7, C -> 2, H -> 4, W -> 9, K -> 5, R -> 7, O -> 6, D -> 3, Z -
                                                  //| > 9, S -> 7)
	
	def wordCode(word: String): String = word.toUpperCase map charCode
                                                  //> wordCode: (word: String)String
	
	val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()
                                                  //> wordsForNum  : Map[String,Seq[String]] = Map(63972278 -> List(newscast), 292
                                                  //| 37638427 -> List(cybernetics), 782754448 -> List(starlight), 2559464 -> List
                                                  //| (allying), 862532733 -> List(uncleared), 365692259 -> List(enjoyably), 86843
                                                  //| 7 -> List(unties), 33767833 -> List(deportee), 742533 -> List(picked), 33646
                                                  //| 46489 -> List(femininity), 3987267346279 -> List(extraordinary), 7855397 -> 
                                                  //| List(pulleys), 67846493 -> List(optimize), 4723837 -> List(grafter), 386583 
                                                  //| -> List(evolve), 78475464 -> List(Stirling), 746459 -> List(singly), 847827 
                                                  //| -> List(vistas), 546637737 -> List(lionesses), 28754283 -> List(curlicue), 8
                                                  //| 4863372658 -> List(thunderbolt), 46767833 -> List(imported), 26437464 -> Lis
                                                  //| t(angering, cohering), 8872267 -> List(turbans), 77665377 -> List(spoolers),
                                                  //|  46636233 -> List(homemade), 7446768759 -> List(rigorously), 74644647 -> Lis
                                                  //| t(ringings), 633738 -> List(offset), 847825 -> List(visual), 772832 -> List(
                                                  //| Pravda), 4729378 -> List
                                                  //| Output exceeds cutoff limit.
	
	def encode(num: String): Set[List[String]] = {
		if(num.isEmpty) Set(List())
		else{
			for {
				split <- 1 to num.length
				word <- wordsForNum(num take split)
				rest <- encode(num drop split)
			}
			yield word :: rest
		}.toSet
	}                                         //> encode: (num: String)Set[List[String]]
	
	def translate(num: String): Set[String] = {
		encode(num) map (_ mkString " ")
	}                                         //> translate: (num: String)Set[String]
	
	translate("7225247386")                   //> res0: Set[String] = Set(sack air fun, pack ah re to, pack bird to, Scala ir
                                                  //| e to, Scala is fun, rack ah re to, pack air fun, sack bird to, rack bird to
                                                  //| , sack ah re to, rack air fun)


	
	//book database
	case class Book (title: String, authors:List[String])
	
	val books: List[Book] = List(
		Book(title = "ABC", authors = List("A", "B", "C")),
		Book(title = "DEF", authors = List("B", "E", "F")),
		Book(title = "GHC", authors = List("C", "H", "I")),
		Book(title = "JKL", authors = List("J", "K", "C"))
	)                                         //> books  : List[example.telephoneletters.Book] = List(Book(ABC,List(A, B, C))
                                                  //| , Book(DEF,List(B, E, F)), Book(GHC,List(C, H, I)), Book(JKL,List(J, K, C))
                                                  //| )
	
	//query db
	//get books by author 'B'
	for(b <- books; a <- b.authors if a startsWith ("B")) yield b.title
                                                  //> res1: List[String] = List(ABC, DEF)

	//get books with 'C' in the title
	for(b <- books if b.title contains ("C")) yield b.title
                                                  //> res2: List[String] = List(ABC, GHC)
	
	//get authors with at least two books - this returns duplicates
	for (b1 <- books; b2 <- books; if b1 != b2 ; a1 <- b1.authors; a2 <- b2.authors; if a1 == a2) yield a1
                                                  //> res3: List[String] = List(B, C, C, B, C, C, C, C)
	
	//make distinct <-  not working right
	//{for (b1 <- books; b2 <- books; if b1 != b2 ; a1 <- b1.authors; a2 <- b2.authors; if a1 == a2) yield a1}.distinct
	
	//instead, if db is a Set, results will also be Sets
	val bookSet: Set[Book] = Set(
		Book(title = "ABC", authors = List("A", "B", "C")),
		Book(title = "DEF", authors = List("B", "E", "F")),
		Book(title = "GHC", authors = List("C", "H", "I")),
		Book(title = "JKL", authors = List("J", "K", "C"))
	)                                         //> bookSet  : Set[example.telephoneletters.Book] = Set(Book(ABC,List(A, B, C))
                                                  //| , Book(DEF,List(B, E, F)), Book(GHC,List(C, H, I)), Book(JKL,List(J, K, C))
                                                  //| )
	
	//get authors with at least two books <- no duplicates
	for (b1 <- bookSet; b2 <- bookSet; if b1 != b2 ; a1 <- b1.authors; a2 <- b2.authors; if a1 == a2) yield a1
                                                  //> res4: scala.collection.immutable.Set[String] = Set(B, C)

	//use higher order functions instead of for expressions
	//get books by author 'B'
	for(b <- books; a <- b.authors if a startsWith ("B")) yield b.title
                                                  //> res5: List[String] = List(ABC, DEF)
	//higher order <- since there are two generators (b <- books; a <- b.authors), thus, use flatMap, use either filter or withFilter
	books flatMap (b => b.authors withFilter (a => a startsWith "B") map (y => b.title))
                                                  //> res6: List[String] = List(ABC, DEF)
	
	//get books with 'C' in the title
	for(b <- books if b.title contains "C") yield b.title
                                                  //> res7: List[String] = List(ABC, GHC)
  //higher order
  books filter (_.title contains "C") map (_.title)
                                                  //> res8: List[String] = List(ABC, GHC)
  
  
  //for expressions work with things other than collections
}