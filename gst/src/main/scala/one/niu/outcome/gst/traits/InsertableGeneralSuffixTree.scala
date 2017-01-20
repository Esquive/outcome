package one.niu.outcome.gst.traits

/**
  * Created by ericfalk on 31/10/2016.
  */
trait InsertableGeneralSuffixTree[T] {

  //TODO: This will have to be Thread safe
  def insert(word : scala.collection.mutable.Iterable[T])

  protected def resetInsert()
}
