package one.niu.outcome.gst

import scala.collection.mutable
import scala.util.control.Breaks._

//TODO: Add a parent edge pointer
/**
  * Created by ericfalk on 31/10/2016.
  */
class Node[T](var children: mutable.LinkedHashMap[T, Edge[T]] = mutable.LinkedHashMap.empty[T, Edge[T]],
              var parentSuffixCount: Int = 0) {

  //Set the parent of the edges
  {
    children.foreach(_._2.parent = this)
  }

  var suffixLink: Node[T] = null

  def this(node: Node[T]) {
    this(node.children)
  }

  def getNextEdge(currentEdge: Edge[T]): Edge[T] = {

    val it = children.iterator
    var nextEdge: Edge[T] = null
    breakable {
      while (it.hasNext) {
        var tuple = it.next()
        if (tuple._2 == currentEdge) {
          if ( {tuple = it.next
            tuple != null} ) {
            nextEdge = tuple._2
            break
          }
          else {
            throw new IllegalStateException()
            //TODO: Maybe we need to revisit here what if the iterator has no next Throw an Anomaly?
          }
        }
      }
    }
    nextEdge
  }

}

class ActiveNode[T](var activeNode: Node[T]) {
  var activeEdge: Edge[T] = null
  var activeLength: Int = 0
}
