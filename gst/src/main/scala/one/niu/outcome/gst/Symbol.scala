package one.niu.outcome.gst

/**
  * Created by ericfalk on 12/11/2016.
  */
class Symbol[T](val content: T) {}

class EndSymbol(val suffixId: String) extends Symbol[String](suffixId) {
  var index: Int = -1

  override def hashCode(): Int = suffixId.hashCode
}
