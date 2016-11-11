package one.niu.outcome.gst

import scala.collection.mutable


/**
  * Created by ericfalk on 31/10/2016.
  */
class Node[T](var children: mutable.LinkedHashMap[T,Edge[T]] = mutable.LinkedHashMap.empty[T,Edge[T]]) {

  var suffixLink : Node[T] = null

    def this(node: Node[T]){
    this(node.children)
  }
}

class ActiveNode[T](var activeNode: Node[T]){
  var activeEdge : Option[T] = None
  var activeLength : Int = 0
}
