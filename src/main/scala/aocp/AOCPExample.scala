package aocp

///////////////////////////////////

import org.mitlware.hyperion3.immutable._
import org.mitlware.hyperion3.immutable.accept._
import org.mitlware.hyperion3.immutable.perturb._
import org.mitlware.hyperion3.immutable.isfinished._

import com.containant._
import com.containant.heuristics._
import com.containant.casestudies._

import cats.data.State
import monocle._

import org.mitlware.solution.permutation.ArrayForm
import org.mitlware.problem.tsp._

import org.mitlware.support.math.ClosedInterval

///////////////////////////////////

object AOCPExample {
  
  val MetaNumFitnessEvaluations = 10000
  val HyperNumFitnessEvaluations = 50
 
  /////////////////////////////////
  
  // Configuration
  val _seed: Int = RNG.instance.seed.toInt
  val _runs: Int = 10
  
  object Hmma extends AntHeuristic {
    override val _maxPheromone: Double = 10
    override val _evaporationRate: Double = 0.4
    override val _antNumber: Int = 1
    override val _iterations: Int = HyperNumFitnessEvaluations / _antNumber
    override val _minimumFraction: Double = 0.10
    override val _recursionDepth: Int = 8
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "mma"
  }
  
  object Hgre extends GrEvoHeuristic {
    override val _population: Int = 10
    override val _length: Int = 9
    override val _maxChoice: Int = 7
    override val _tournamentSize = 5
    override val _generations = HyperNumFitnessEvaluations / _population
    override val _recursionDepth = 8
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "gre"
  }

  object Hran extends RandomHeuristic {
    override val _iterations = HyperNumFitnessEvaluations
    override val _recursionDepth = 8
    override val RNG: java.util.Random = new java.util.Random(_seed)
    override def toString: String = "ran"
  }

  //////////////////////////////////////////////////////////////////////

  def getInstance(id: Int): TSP.TSPLibInstance =
   tspInstances(id).instance

  def getName(tsp: TSP.TSPLibInstance): String = {
    val instance_text = tsp.toString()
    instance_text.substring(instance_text.indexOf("name")+5, instance_text.indexOf("type")-4)
  }

  /////////////////////////////////
  
  // Problem Description

  def temperatureRange(initial: ArrayForm, eval: ArrayForm => Double, numSamples: Int): ClosedInterval = {
    var current = initial.clone()
    val randomWalkFitness = for( i <- 0 until numSamples ) yield {
      val result = eval(current)
      current.randomSwap(RNG.instance.self)
      result
    }
    
    val allEq = randomWalkFitness.tail.forall { f => f == randomWalkFitness.head }
    if( allEq )
      ClosedInterval.create(0.0, randomWalkFitness.head / 2.0)
    else {
      import org.mitlware.support.temperature.WhiteCoolingScheduleParameters._
      WhiteTemperatureRangeForSA(randomWalkFitness.toArray)
    }
  }

  /////////////////////////////////
  
  case class InitialTemperature(asDouble: Double) {
    require(asDouble >= 0.0 )    
  }
  
	case class MyEnv(iter: Iter, 
    maxIter: MaxIter, 
    temperature: Temperature,
    initialTemperature: InitialTemperature,    
    tourLength: Evaluate[MyEnv,ArrayForm,Double])

  object MyEnv {

	  def apply(
      maxIter: MaxIter, 
      initialTemperature: InitialTemperature, 
      tourLength: Evaluate[MyEnv,ArrayForm,Double]): MyEnv = 
        MyEnv(
          Iter(0),
          maxIter, 
          Temperature(initialTemperature.asDouble),
          initialTemperature,
          tourLength )
	}
    
  /////////////////////////////////

	def readTSP(f: java.io.File): TSP.TSPLibInstance = 
    new TSP.TSPLibInstance( new java.io.FileInputStream( f ) ) 
	
	case class Problem(name: String, instance: TSP.TSPLibInstance, optimalTourLength: Double) {
	  def relativeError(x: ArrayForm): Double = {
	    require( x.size() == instance.numCities() )
	    val tourLength = TSP.tourLength(x,instance.getDistanceFn())
	    ((tourLength - optimalTourLength)/optimalTourLength)
	  } ensuring { result => result >= 0.0 && result <= 1.0 }
	}
	
  def eval(problem: Problem)(x: ArrayForm): Double = {  
    TSP.tourLength(x,problem.instance.getDistanceFn())
  }
    
	case class EvalS(problem: Problem) extends Evaluate[MyEnv,ArrayForm,Double] {
    override def apply(x: ArrayForm) = State[MyEnv,Double] { env => (env,eval(problem)(x)) }
  }

	/////////////////////////////////
	
  case class LinearCoolingSchedule[Env](
    initialTemperature: monocle.Lens[Env,InitialTemperature],
    temperature: monocle.Lens[Env,Temperature],iter: Lens[Env,Iter],maxIter: Lens[Env,MaxIter]) extends CoolingSchedule[Env] {
    
      override def apply: State[Env,Temperature] = State[Env,Temperature] { env =>
      val newTemperature = Temperature( org.mitlware.support.math.LinearInterpolation.apply(iter.get(env).asLong, 
        0, 1 + maxIter.get(env).asLong, 
        initialTemperature.get(env).asDouble, 0.0 ) )
      val newEnv = temperature.set(newTemperature)(env)
      (newEnv, newTemperature)
    }
  }

  ///////////////////////////////////

  case class AcceptMetropolisHastings[Env,Sol](isMinimizing: Boolean, 
      evaluate: Lens[Env, Evaluate[Env,Sol,Double]], 
      schedule: CoolingSchedule[Env]) extends Accept[Env,Sol] {
    
    override def apply(incumbent: Sol, incoming: Sol): State[Env,Sol] = for {
      env <- State.get[Env]
      incumbentValue <- evaluate.get(env).apply(incumbent);
      incomingValue <- evaluate.get(env).apply(incoming) 
      temperature <- schedule.apply;
      val acceptProb: Double = 1.0 / ( 1.0 + Math.exp(( if (isMinimizing) incomingValue - incumbentValue else incumbentValue - incomingValue) ) / temperature.asDouble )
    } yield 
      if( RNG.instance.nextDouble < acceptProb ) incoming else incumbent
  }
	
  /////////////////////////////////
  
	object TSPModule extends Module {

  	val iterLens: monocle.Lens[MyEnv, Iter] = monocle.macros.GenLens[MyEnv] { _.iter }
	  val maxIterLens: monocle.Lens[MyEnv, MaxIter] = monocle.macros.GenLens[MyEnv] { _.maxIter }
    val initialTemperatureLens: monocle.Lens[MyEnv, InitialTemperature] = monocle.macros.GenLens[MyEnv] { _.initialTemperature }
    val temperatureLens: monocle.Lens[MyEnv, Temperature] = monocle.macros.GenLens[MyEnv] { _.temperature }   
	  val tourLengthLens: monocle.Lens[MyEnv, Evaluate[MyEnv,ArrayForm,Double]] = monocle.macros.GenLens[MyEnv] { _.tourLength }

	  /////////////////////////////// GRAMMAR SECTION
      
    val randomSwap: RandomSwap[MyEnv] = RandomSwap[MyEnv]()
    val randomInsert: RandomInsert[MyEnv] = RandomInsert[MyEnv]()    
    val randomShuffle: RandomShuffle[MyEnv] = RandomShuffle[MyEnv]()
    def randomShuffleSubset(mutationStrength: MutationStrength): RandomShuffleSubset[MyEnv] = 
      RandomShuffleSubset[MyEnv](mutationStrength)    
    def reverseSubtours(mutationStrength: MutationStrength): ReverseSubtours[MyEnv] = 
      ReverseSubtours[MyEnv](mutationStrength)

    val acceptImproving: AcceptImproving[MyEnv,ArrayForm,Double] = 
      AcceptImproving( isMinimizing=true,scala.math.Ordering.Double, 
          tourLengthLens)      
      
    val accept: Accept[MyEnv,ArrayForm] = AcceptImprovingOrEqual(isMinimizing=true, 
        scala.math.Ordering.Double, tourLengthLens)
      
    val acceptImprovingOrEqual: AcceptImprovingOrEqual[MyEnv,ArrayForm,Double] =
      AcceptImprovingOrEqual(isMinimizing=true,scala.math.Ordering.Double, tourLengthLens)      

    val linearCoolingSchedule: LinearCoolingSchedule[MyEnv] =
      LinearCoolingSchedule(initialTemperatureLens,temperatureLens,iterLens,maxIterLens)
      
    def geometricCoolingSchedule(ratio: CoolingRatio): GeometricCoolingSchedule[MyEnv] =
      GeometricCoolingSchedule(ratio, temperatureLens)
    
    def acceptMetropolisHastings(schedule: CoolingSchedule[MyEnv]): AcceptMetropolisHastings[MyEnv,ArrayForm] =
        AcceptMetropolisHastings[MyEnv,ArrayForm](
            isMinimizing=true, 
            tourLengthLens, 
            schedule)

	  val iterGreaterThanMaxIter: IterGreaterThanMaxIter[MyEnv,ArrayForm] =
	    IterGreaterThanMaxIter(iterLens,maxIterLens)

    def iteratedPerturbation(
      perturb: Perturb[MyEnv,ArrayForm], 
      accept: Accept[MyEnv,ArrayForm], 
      finished: Condition[MyEnv,ArrayForm]): IteratedPerturbation[MyEnv,ArrayForm] =
        IteratedPerturbation[MyEnv,ArrayForm](iterLens,perturb,accept,finished)
        
    /////////////////////////////// ERC SECTION

    val coolingRatio1 = CoolingRatio(0.1)
    val coolingRatio2 = CoolingRatio(0.2)    
    val coolingRatio3 = CoolingRatio(0.3)
    val coolingRatio4 = CoolingRatio(0.4)
    val coolingRatio5 = CoolingRatio(0.5)
    val coolingRatio6 = CoolingRatio(0.6)
    val coolingRatio7 = CoolingRatio(0.7)
    val coolingRatio8 = CoolingRatio(0.8)
    val coolingRatio9 = CoolingRatio(0.9)
    
    val mutationStrength1 = MutationStrength(0.0)
    val mutationStrength2 = MutationStrength(0.1)
    val mutationStrength3 = MutationStrength(0.2)    
    val mutationStrength4 = MutationStrength(0.3)
    val mutationStrength5 = MutationStrength(0.4)
    val mutationStrength6 = MutationStrength(0.5)
    val mutationStrength7 = MutationStrength(0.6)
    val mutationStrength8 = MutationStrength(0.7)
    val mutationStrength9 = MutationStrength(0.8)
    val mutationStrength10 = MutationStrength(0.9)
  }
  
  /////////////////////////////////

  def nearestNeighbour(tsp: TSP.TSPLibInstance, startCity: Int): ArrayForm = {
    require( startCity >= 1 && startCity <= tsp.numCities() )
    var unvisited = (1 to tsp.numCities() ).toList.filter { _ != startCity }

		var tour = List(startCity)
		while( tour.length < tsp.numCities() ) {
		  val nearest = unvisited.minBy { v => tsp.getDistanceFn()(tour.last, v) }
		  unvisited = unvisited.filter { _ != nearest }
		  tour = tour :+ nearest 
		}
		
		new ArrayForm( tour.map { _ - 1 }.toArray:_* )
	}

  def bestNearestNeighbour(tsp: TSP.TSPLibInstance): ArrayForm = {
		var bestStart = (1 to tsp.numCities() ).minBy { startCity => 
      TSP.tourLength(nearestNeighbour(tsp, startCity), tsp.getDistanceFn() ) 
    }
		nearestNeighbour(tsp, bestStart)
  }

  /////////////////////////////////
  
  def relativeErrorInstance(problem: Problem)(solution: IteratedPerturbation[MyEnv,ArrayForm]): Double = {
    val initial = nearestNeighbour(problem.instance,1)    
    val tempRange = temperatureRange(initial, eval( problem ) _, numSamples=100)
 
	  val initialEnv = MyEnv(MaxIter(MetaNumFitnessEvaluations),InitialTemperature(tempRange.getUpper), 
	      EvalS(problem) )

    RNG.instance.setSeed(RNG.defaultSeed)
    val bestTour = solution( initial ).runA(initialEnv).value
    problem.relativeError(bestTour)
  }
  
  /////////////////////////////////
  
  var iter = 0
  val outputEveryIter = 200
  var best =  Option.empty[(IteratedPerturbation[MyEnv,ArrayForm],Double)]
  
  def fitnessImpl(instances: List[Problem])(solution: IteratedPerturbation[MyEnv,ArrayForm]): Double = {

    val result = instances.map(relativeErrorInstance(_)(solution)).sum / instances.size.toDouble
    if( best.isEmpty || result < best.get._2 ) {
      best = Some(solution, result )
      jeep.lang.Diag.println( s"iter: $iter, newBest: $best" )
    }

    iter += 1
    1.0 - result
  }

  var cache = Map.empty[IteratedPerturbation[MyEnv,ArrayForm],Double]
  
  def fitness(instances: List[Problem])(solution: IteratedPerturbation[MyEnv,ArrayForm]): Double = {
    if( cache.contains( solution ) )
      cache(solution)
      else {
        val f = fitnessImpl(instances)(solution)
        cache = cache.updated(solution, f )
        f
      }
  }
  
  //////////////////////////////////////////////////////////////////////
  // Experiment Details

  import java.io.File

  def getRecursiveListOfFiles(dir: File): List[File] = {
    val these = dir.listFiles.toList
    these.filter(!_.isDirectory) ++ 
      these.filter(_.isDirectory).flatMap(getRecursiveListOfFiles)
  }

  /////////////////////////////////
  
  def tspFileForOptTour(f: File): Option[File] = {
    val pathStr = f.getAbsolutePath()    
    require( pathStr.endsWith( ".opt.tour" ) )
    val t = new java.io.File( pathStr.replace( ".opt.tour", ".tsp"  ) )
    if( t.exists() && !t.isDirectory() ) Some(t) else None    
  }
    
  def optTourFileForTSP(f: File): Option[File] = {
    val pathStr = f.getAbsolutePath()    
    require( pathStr.endsWith( ".tsp" ) )
    val o = new java.io.File( pathStr.replace( ".tsp", ".opt.tour" ) )
    if( o.exists() && !o.isDirectory() ) Some(o) else None
  }
  
  def optimumTourLengthFromFile(f: java.io.File, tsp: TSP.TSPLibInstance): Option[Double] = {
    org.mitlware.support.lang.Diag.println( f )
    for { 
      listOfLines <- optTourFileForTSP( f ).map { f => scala.io.Source.fromFile(f).getLines.toList }
      start <- listOfLines.zipWithIndex.find { case (line,index) => line == "TOUR_SECTION" };
      end   <- listOfLines.zipWithIndex.find { case (line,index) => line == "EOF" }      
    } yield {
      val startIndex = start._2
      val endIndex = end._2
      assert( startIndex < endIndex ) // FIXME
      
      val sublist = listOfLines.slice(startIndex + 1, endIndex - 1).map { _.trim.toInt }
      val optTourList = if( sublist.last == -1 ) sublist.dropRight(1) else sublist
      val optTourListZeroIndexed = optTourList.map { _ - 1} 
      val optTour = new ArrayForm( optTourListZeroIndexed.toArray:_* )
      val optTourCost = TSP.tourLength(optTour, tsp.getDistanceFn() )
      optTourCost
    }
  }

  // From: https://www.iwr.uni-heidelberg.de/groups/comopt/software/TSPLIB95/STSP.html
  def optimumTourLength(f: java.io.File): Option[Double] = {
    
    val optimalLengths = Map.empty[String,Int] ++ List( 
      "a280" -> 2579, "ali535" -> 202339,"att48" -> 10628,"att532" -> 27686,"bayg29" -> 1610,"bays29" -> 2020,
      "berlin52" -> 7542,"bier127" -> 118282,"brazil58" -> 25395,"brd14051" -> 469385,"brg180" -> 1950,"burma14" -> 3323,
      "ch130" -> 6110,"ch150" -> 6528,"d198" -> 15780,"d493" -> 35002,"d657" -> 48912,"d1291" -> 50801,"d1655" -> 62128,
      "d2103" -> 80450,"d15112" -> 1573084,"d18512" -> 645238,"dantzig42" -> 699,
      // "dsj1000" -> 18659688, // (EUC_2D) , "dsj1000" -> 18660188 // (CEIL_2D)
	    "eil51" -> 426,"eil76" -> 538,"eil101" -> 629,"fl417" -> 11861,"fl1400" -> 20127,"fl1577" -> 22249,
      "fl3795" -> 28772,"fnl4461" -> 182566,"fri26" -> 937,"gil262" -> 2378,"gr17" -> 2085,"gr21" -> 2707,
      "gr24" -> 1272,"gr48" -> 5046,"gr96" -> 55209,"gr120" -> 6942,"gr137" -> 69853,"gr202" -> 40160,
      "gr229" -> 134602,"gr431" -> 171414,"gr666" -> 294358,"hk48" -> 11461,"kroA100" -> 21282,
      "kroB100" -> 22141,"kroC100" -> 20749,"kroD100" -> 21294,"kroE100" -> 22068,"kroA150" -> 26524,
      "kroB150" -> 26130,"kroA200" -> 29368,"kroB200" -> 29437,"lin105" -> 14379,"lin318" -> 42029,
      "linhp318" -> 41345,"nrw1379" -> 56638,"p654" -> 34643,"pa561" -> 2763,"pcb442" -> 50778,
      "pcb1173" -> 56892,"pcb3038" -> 137694,"pla7397" -> 23260728,"pla33810" -> 66048945,
      "pla85900" -> 142382641,"pr76" -> 108159,"pr107" -> 44303,"pr124" -> 59030,"pr136" -> 96772,
      "pr144" -> 58537,"pr152" -> 73682,"pr226" -> 80369,"pr264" -> 49135,"pr299" -> 48191,
      "pr439" -> 107217,"pr1002" -> 259045,"pr2392" -> 378032,"rat99" -> 1211,"rat195" -> 2323,
      "rat575" -> 6773,"rat783" -> 8806,"rd100" -> 7910,"rd400" -> 15281,"rl1304" -> 252948,
      "rl1323" -> 270199,"rl1889" -> 316536,"rl5915" -> 565530,"rl5934" -> 556045,
      "rl11849" -> 923288,"si175" -> 21407,"si535" -> 48450,"si1032" -> 92650,
      "st70" -> 675,"swiss42" -> 1273,"ts225" -> 126643,"tsp225" -> 3916,"u159" -> 42080,
      "u574" -> 36905,"u724" -> 41910,"u1060" -> 224094,"u1432" -> 152970,"u1817" -> 57201,
      "u2152" -> 64253,"u2319" -> 234256,"ulysses16" -> 6859,"ulysses22" -> 7013,
      "usa13509" -> 19982859,"vm1084" -> 239297,"vm1748" -> 336556 )

      optimalLengths.get( f.getName().dropRight( ".tsp".length ) ).map { _.toDouble }
  }
  
  /////////////////////////////////
  
  def report(problems: List[Problem])(alg: IteratedPerturbation[MyEnv,ArrayForm]): Unit = {
    
    def solve(problem: Problem)(solution: IteratedPerturbation[MyEnv,ArrayForm]): ArrayForm = {

      val initial = bestNearestNeighbour(problem.instance)      
      val tempRange = temperatureRange(initial, eval( problem ) _, numSamples=10)
      val initialEnv = MyEnv(
          MaxIter(MetaNumFitnessEvaluations),
          InitialTemperature(tempRange.getUpper), 
          EvalS(problem) )
	    solution( initial ).runA(initialEnv).value
    }
    
    val solutions = problems.zipWithIndex.map { case (p,index) =>
      org.mitlware.support.lang.Diag.println( s"solving ${p.name} ($index of ${problems.size}" ) 
      val solution = solve(p)(alg)
      org.mitlware.support.lang.Diag.println( s"${p.relativeError(solution)}" ) 
      (p.name,solution) 
    }
    
    val tourLengths = problems.zip(solutions).map { case (problem,solution) => TSP.tourLength(solution._2, problem.instance.getDistanceFn()) }
    org.mitlware.support.lang.Diag.println( tourLengths )    
    val relativeErrors = problems.zip(solutions).map { case (problem,solution) => (problem.name,problem.relativeError(solution._2)) }
    org.mitlware.support.lang.Diag.println( s"R.E.: $relativeErrors, Sigma R.E.: ${relativeErrors.unzip._2.sum}" )
    
    val (names,re) = relativeErrors.map { p => ( "\"" + p._1.dropRight(".tsp".length ) + "\"",p._2) }.unzip
    // val barCharStr = s"""BarChart[{${re.mkString(",")}}, ChartLabels -> (Rotate[#, Pi/2] & /@ { ${names.mkString(",")} })]"""
    // println( barCharStr )
    
    val mean = org.apache.commons.math3.stat.StatUtils.mean( relativeErrors.unzip._2.toArray )
    val min = org.apache.commons.math3.stat.StatUtils.min( relativeErrors.unzip._2.toArray )
    val max = org.apache.commons.math3.stat.StatUtils.max( relativeErrors.unzip._2.toArray )
    val variance = org.apache.commons.math3.stat.StatUtils.variance( relativeErrors.unzip._2.toArray )
    val sd = math.sqrt(variance)    
    org.mitlware.support.lang.Diag.println( 
      s"R.E.s: min: ${min}, max: ${max}, mean: ${mean}, variance: ${variance}, sd: $sd" 
    )
  }

  /////////////////////////////////
    
  def isSmallInstance(p: Problem) = p.instance.numCities() <= 100
  def isLargeInstance(p: Problem) = p.instance.numCities() >= 5000  
    
  def trainAndTest(trainingSet: List[Problem], testSet: List[Problem]): Unit = {
    
    println("\n-----------------------------")
    println("Case Study: TSP")
    println("Runs: " + _runs)
    
    ///////////////////////////////
    
    iter = 0
		best = None
		cache = Map.empty[IteratedPerturbation[MyEnv,ArrayForm],Double]

    val startTime = System.currentTimeMillis()
    
    val experiments = ContainAntHyperHeuristicFramework.experimentN(
      List(Hgre/*, Hran, Hmma*/),
      runs = _runs,
      TSPModule, 
      fitness(trainingSet) _ 
    )
    
    val endTime = System.currentTimeMillis()      
      
    // println( experiments )
    val bestResult = experiments.results.flatten.maxBy { _.fitness }
    // val tourLength = 1.0/best.fitness
    org.mitlware.support.lang.Diag.println( s"best solution: $bestResult" )
    println( s"Training took: ${(endTime - startTime)/1000.0}s" )
      
    ///////////////////////////////

    println( "Testing:" )
    report(testSet)(bestResult.solution)
  }
  
  /////////////////////////////////  
  
  lazy val tspInstances: List[Problem] = {  
    val root = new File(System.getProperty("user.dir") + 
      File.separatorChar + "resources" + File.separatorChar + "TSPLIB95" + File.separatorChar + "tsp" )
    
    // val allOptimalTourFiles = getRecursiveListOfFiles( root ).filter { f => f.getAbsolutePath().endsWith(".opt.tour") }
    // val tspFilesWithOptimalTour = allOptimalTourFiles.map { tspFileForOptTour(_) }.flatten
    val allTSPFiles = getRecursiveListOfFiles( root ).filter { f => f.getAbsolutePath().endsWith(".tsp") }    
    val tspFilesWithOptimalTour = allTSPFiles.map { f => optimumTourLength(f).map { x => f } }.flatten    

    val parsedTSPInstances = tspFilesWithOptimalTour.map { f =>
      try {
        val tsp = readTSP(f)
        val numCities = tsp.numCities()
        val initial = new ArrayForm( numCities )
        TSP.tourLength(initial, tsp.getDistanceFn())        
        ( f, Some( tsp ) )        
      }
      catch {
        case _: Throwable => 
          // println( "cannot read:" + f ) 
          ( f, None )        
      }
    }
  
    parsedTSPInstances.filter { p => p._2.isDefined /* && p._2.get.numCities() <= 100 */ }.map { 
      p => (p._1,p._2.get)
      }.map { case (file,tsp) => Problem(file.getName(),tsp,optimumTourLength(file).get ) 
    }
  }
  
  /////////////////////////////////  
  
  def main(args: Array[String]): Unit = {
  
    org.mitlware.support.lang.Diag.println (new java.util.Date() )
    
    ///////////////////////////////
    
    var validTSPInstances = tspInstances 
    org.mitlware.support.lang.Diag.println( "#instances: " + validTSPInstances.length + ":" + validTSPInstances.map { _.name } )    

    ///////////////////////////////
    
    val trainingSet = validTSPInstances.filter { isSmallInstance }
    val testSet = validTSPInstances.filter { p => !isSmallInstance(p) && p.instance.numCities() <= 4000 }
     
    println( s"trainingSet: " + trainingSet.map { x => x.name }.mkString(",") )
    println( s"testingSet: " + testSet.map { x => x.name }.mkString(",") )
      
    trainAndTest( trainingSet, testSet )
  }
}

// End ///////////////////////////////////////////////////////////////
