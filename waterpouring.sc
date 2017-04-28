package example

object waterpouring {

  //water pouring problem
  //two glasses capable of containing 4 & 9 l respectively
  //can fill unlimited from faucet, can empty into sink, can transfer between glasses
  //get 6 l exactly
  //more generically, imagine any number of glasses, and a given amount as target
  //ans: generate all possible paths using all possible moves
  //until we've found it or exhausted our search space with target not reachable
  
  class Pouring(capacities: Vector[Int]) {
  	//states
  	type State = Vector[Int]
  	val initState = capacities map (x => 0)
  	
  	//moves
  	trait Move {def change(state: State): State}
  	
  	case class Empty(glass: Int) extends Move {
  		def change(state: State): State = state updated (glass, 0)
  	}
  	
  	case class Fill(glass: Int) extends Move {
  		def change(state: State): State = state updated (glass, capacities(glass))
  	}
  	
  	case class Pour(from: Int, to: Int) extends Move {
  		def change(state: State): State = {
  			val amt = state(from) min (capacities(to) - state(to))
  			state updated (from, state(from) - amt) updated (to, state(to) + amt)
  		}
  	}
  	
  	val glasses = 0 until capacities.length
  	
  	//all possible moves
  	val moves =
  			(for(g <- glasses) yield Empty(g)) ++
  			(for(g <- glasses) yield Fill(g)) ++
  			(for(from <- glasses; to <- glasses; if from != to) yield Pour(from, to))
  			
/*  class Path (history: List[Move]) {
	  	def endState: State = trackState(history)
	  	
	  	private def trackState(xs: List[Move]): State = xs match {
	  		case Nil => initState
	  		case move :: xs1 => move change trackState(xs1)
	  	}
	  	
	  	//the above method is similar to foldRight, so can do following instead:
	  	def LendState: State = (history foldRight initState) (_ change _)
	  	
	  	def extend(move:Move) = new Path(move :: history)
	  	
	  	override def toString = (history.reverse mkString " ") + "-> " + LendState
	  }*/

		//the problem with the above Path class is that endState is recomputed over and over again
		
		//the point of having a val parameter is that val makes it a global variable  			
	  class Path (history: List[Move], val endState: State) {
	  
	  	def extend(move:Move) = new Path(move :: history, move change endState)
	  	
	  	override def toString = (history.reverse mkString " ") + "-> " + endState
	  }
	  
	  val initPath = new Path(Nil, initState)
	  
  	def from (paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
  		if(paths.isEmpty) Stream.empty
  		else{
  			val more = for {
  				path <- paths
  				next <- moves map path.extend
  				if !(explored contains next.endState)			//exclude states which have been explored before
  			} yield next
  			paths #:: from(more, explored ++ (more map (_.endState)))
  		}
  	}
  	
  	//all possible paths : search space
  	val pathSets = from(Set(initPath), Set(initState))
  	
  	//actual solution
  	def soln (target: Int): Stream[Path] = {
  		for{
  			pathSet <- pathSets
  			path <- pathSet
  			if(path.endState contains target)
  		} yield path
  	}
  }
  
  val Problem = new Pouring(Vector(4,9))          //> Problem  : example.waterpouring.Pouring = example.waterpouring$Pouring@1597
                                                  //| 5490
  
  Problem.moves                                   //> res0: scala.collection.immutable.IndexedSeq[Product with Serializable with 
                                                  //| example.waterpouring.Problem.Move] = Vector(Empty(0), Empty(1), Fill(0), Fi
                                                  //| ll(1), Pour(0,1), Pour(1,0))

	Problem.pathSets.take(2).toList           //> res1: List[Set[example.waterpouring.Problem.Path]] = List(Set(-> Vector(0, 
                                                  //| 0)), Set(Fill(0)-> Vector(4, 0), Fill(1)-> Vector(0, 9)))

	Problem.soln(6)                           //> res2: Stream[example.waterpouring.Problem.Path] = Stream(Fill(1) Pour(1,0) 
                                                  //| Empty(0) Pour(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0)-> Vector(4, 6), ?)
                                                  //| 
	
	//this only gives the first solution, which may not be the shortest solution 
}