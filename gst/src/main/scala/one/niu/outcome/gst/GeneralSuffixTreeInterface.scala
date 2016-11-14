package one.niu.outcome.gst

/**
  * Created by ericfalk on 31/10/2016.
  */
trait GeneralSuffixTreeInterface[T] {

  def insert(word : scala.collection.mutable.Iterable[T])

}
