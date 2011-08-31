package mobi.pereira.`hanoï-core`

object Hanoï {

  def move(n: Int, from: Tower, to: Tower, tmp: Tower): List[Move] = {
    // @annotation.tailrec
    def moveTC(n: Int, from: Tower, to: Tower, tmp: Tower, accu: List[Move]): List[Move] =
      n match {
	case 1 => (from, to)::accu
	case _ => {
	  val moves = moveTC(n-1, from, tmp, to, accu)
	  moveTC(n-1, tmp, to, from, (from, to)::moves)
	}
      }
    moveTC(n, from, to, tmp, Nil)
  }

  def resolve(numberOfDiscus: Int): List[Move] = {
    if(numberOfDiscus < 1) Nil
    else move(numberOfDiscus, 1, 3, 2).reverse
  }

  /*
  def main(args: Array[String]) {
    resolve(2) foreach { move => println("Move from "+move._1+" to "+move._2+".") }
  }
  */
}
