package aocp

case class RNG(seed: Long) extends scala.util.Random(seed)

object RNG {
  
  val defaultSeed: Long = 0xDEADBEEF
  
  lazy val instance = RNG(defaultSeed) 
}

// End ///////////////////////////////////////////////////////////////
