package one.niu.outcome.gst

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by ericfalk on 31/10/2016.
  */
class Edge[T](private var label : mutable.Iterable[T]) {

  //TODO: Define a default leaf node type
  var child : Node[T] = null

//  this.label = {
//    mutable.ListBuffer[T](label.toSeq: _*)
//  }

  def append(symbol: T): Unit ={
    label.asInstanceOf[ListBuffer[T]] += symbol
  }

  def getLabel(): mutable.Buffer[T] = {
    return this.label.asInstanceOf[ListBuffer[T]]
  }

  def setLabel(newLabel : mutable.Buffer[T]) = {
    this.label = newLabel
  }


}

class End