package one.niu.outcome.dag

import java.nio.ByteBuffer

/**
  * Created by ericfalk on 16/01/2017.
  */
class Sequence[T](var sequence: T, turnToSymbols: T => Iterable[Array[Byte]], turnToSequence: (Iterable[ByteBuffer], Int) => T) {

  def getSymbols(): Iterable[Array[Byte]] = {
    val result = this.turnToSymbols(sequence)
    result
  }

  def getSequence(iterable: Iterable[ByteBuffer], size: Int): T = {
    val result = this.turnToSequence(iterable,size)
    result
  }

}
