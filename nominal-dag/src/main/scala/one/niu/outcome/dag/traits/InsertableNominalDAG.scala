package one.niu.outcome.dag.traits

import one.niu.outcome.dag.Sequence

/**
  * Created by ericfalk on 16/01/2017.
  */
trait InsertableNominalDAG[T] {

  //TODO: This will have to be thread safe
  def insert(sequence: Sequence[T]): Array[Byte]

}
