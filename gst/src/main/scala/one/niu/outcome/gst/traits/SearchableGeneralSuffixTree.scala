package one.niu.outcome.gst.traits

/**
  * Created by ericfalk on 22/11/2016.
  */
trait SearchableGeneralSuffixTree[T] {

  def search(word : scala.collection.mutable.Iterable[T])

}
