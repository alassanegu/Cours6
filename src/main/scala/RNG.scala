trait RNG {
  def nextInt:(Int,RNG)
}


case class SimpleRNG(seed:Long) extends RNG {
  def nextInt:(Int,RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}



object RNG {
  def nonNegativeInt(rng: RNG):(Int, RNG) = {
    val (i , r) = rng.nextInt
    (if (i<0) (-i+1) else i, r)
  }

  def nonNegativeInt2(rng: RNG): (Int, RNG) = {
    val (i, rnd) = rng.nextInt
    if (i > 0) (i, rnd)
    else if (i == Int.MinValue) (-(i + 1), rnd)
    else (-i, rnd)
  }

  def double(rng:RNG):(Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i / Int.MaxValue.toDouble, r)
  }

  def intDouble(rng0 : RNG): ((Int,Double), RNG) = {
      val (i,rng1) = rng0.nextInt
      val (d,rng2) = double(rng1)
      ((i,d),rng2)
  }

  def double3(r0: RNG): ((Double, Double, Double), RNG) = {
      val (d1,r1) = double(r0)
      val (d2,r2) = double(r1)
      val (d3,r3) = double(r2)
      ((d1,d2,d3),r3)
  }

  def ints(count:Int)(r0: RNG):(List[Int], RNG) = count match {
    case 0  => (Nil,r0)
    case _  => {
      val (head, r1) = r0.nextInt
      val (tail,r2) = ints(count -1)(r1)
      (head::tail,r2)
    }
  }

  def ints1(count: Int)(r0: RNG): (List[Int], RNG) =  {
   if (count <= 0) (Nil, r0)
   else {
     val (head, r1) = r0.nextInt
     val (tail, r2) = ints(count - 1)(r1)
     (head :: tail, r2)
   }
  }



}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world!")
  }
}