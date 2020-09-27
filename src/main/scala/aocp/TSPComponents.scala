package aocp

import cats.data.State
import monocle._

import org.mitlware.hyperion3.immutable._
import org.mitlware.solution.permutation.ArrayForm

///////////////////////////////////

case class RandomInt[Env]() extends Perturb[Env,Int] {

  override def apply(x: Int): State[Env,Int] = 
	  State[Env,Int] { env => (env,RNG.instance.nextInt()) }
}

case class RandomShuffle[Env]() extends Perturb[Env,ArrayForm] {

  private def randomShuffle[Env] = (x: ArrayForm) => State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomShuffle( RNG.instance.self )
    (env,result)
  }
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = 
    randomShuffle[Env](x) 
}

case class RandomSwap[Env]() extends Perturb[Env,ArrayForm] {

  private def randomSwap[Env] = (x: ArrayForm) => State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomSwap( RNG.instance.self )
    (env,result)
  }
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = randomSwap[Env](x)  
}

case class RandomInsert[Env]() extends Perturb[Env,ArrayForm] {

  private def randomInsert[Env] = (x: ArrayForm) => State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomInsert( RNG.instance.self )
    (env,result)
  }
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = randomInsert[Env](x)  
}

case class RandomShuffleSubset[Env](mutationDegree: MutationStrength) extends Perturb[Env,ArrayForm] {
  
  override def apply(x: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.randomShuffleSubset( new org.mitlware.support.math.UnitInterval(mutationDegree.asDouble), RNG.instance.self )
    (env,result)
  }  
}

case class ReverseSubtours[Env](mutationDegree: MutationStrength) extends Perturb[Env,ArrayForm] {

  override def apply(x: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>
    val result = x.clone()
    result.nOpt(new org.mitlware.support.math.UnitInterval(mutationDegree.asDouble), RNG.instance.self )
    (env,result)
  }  
}

case class BestImproving2Opt[Env](tourLengthLens: monocle.Lens[Env, Evaluate[Env,ArrayForm,Double]],maxPasses: Int) 
  extends Perturb[Env,ArrayForm] {

  private def reverseSubtour(x: ArrayForm, i: Int, j: Int): ArrayForm = {
    var upper = j
		while (upper < i) {
			upper += x.size()
		}
		
    val a = x.toArray
		for (k <- 0 until (upper - i + 1) / 2 ) {
			val temp =a(i+k)
			a(i+k ) = a(upper-k)
			a(upper-k) = temp
		}
    new ArrayForm(a:_*)
	}

  // Adapted from https://github.com/dhadka/TSPLIB4J/blob/master/src/org/moeaframework/problem/tsplib
  
  override def apply(tour: ArrayForm): State[Env,ArrayForm] = State[Env,ArrayForm] { env =>  
 
    def evalHackFIXME(x: ArrayForm): Double = 
      tourLengthLens.get( env ).apply(x).runA(env).value
    
		// tours with 3 or fewer nodes are already optimal
		if (tour.size() < 4) {
			(env,tour)		
    }
		else {
		  var incumbent = tour
		  var incumbentValue = evalHackFIXME( incumbent )
		  var modified = true
		  var numPasses = 0
		  
		  while( modified && numPasses < maxPasses ) {
			  modified = false
			  for( i <- 0 until tour.size; j <- i+2 until tour.size ) {
			    
          val incoming = reverseSubtour(incumbent, i+1, j)
          val incomingValue = evalHackFIXME( incoming )
          if( incomingValue < incumbentValue ) {
			  			modified = true            
              incumbentValue = incomingValue
              incumbent = incoming
          }
        }
        numPasses += 1			  
      }
			(env,incumbent)		  
    }
  }
}

// End ///////////////////////////////////////////////////////////////
