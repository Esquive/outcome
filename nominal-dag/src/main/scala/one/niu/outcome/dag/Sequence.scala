package one.niu.outcome.dag

/**
  * Created by ericfalk on 16/01/2017.
  */
class Sequence[T](var sequence: T, turnToSymbols: T => Iterable[Array[Byte]]) {

  def getSymbols(): Iterable[Array[Byte]] = {
    val result = this.turnToSymbols(sequence)
    result
  }

}
