package one.niu.outcome.gst

import scala.collection.mutable
import scala.util.control.Breaks._

//TODO: Add a parent edge pointer
/**
  * Created by ericfalk on 31/10/2016.
  */
class Node[T](private var children: mutable.LinkedHashMap[T, Edge[T]] = mutable.LinkedHashMap.empty[T, Edge[T]],
              var parentSuffixCount: Int = 0) {

  //Set the parent of the edges
  {
    children.foreach(_._2.parent = this)
  }

  val endSymbolEdges = mutable.ListBuffer.empty[Edge[T]]
  var suffixLink: Node[T] = null

  //Constructor
  def this(node: Node[T]) {
    this(node.children)
  }


  def getChild(symbol: Symbol[T]): Edge[T] = {
    return {
      if (symbol.isInstanceOf[EndSymbol]) {
        val result = children.filter(x => {
          x._2.containsEndSymbol(symbol) && x._2.suffixCount == 0
        })
        //TODO: review this logic here.
        if (result.size > 1) {
          throw new IllegalStateException
        } else if(result.size == 0) {
          null
        } else {
          result.getOrElse(symbol.symbol, null)
        }
      } else {
        children.getOrElse(symbol.symbol, null)
      }
    }
  }

  def addChild(symbol : Symbol[T]): Edge[T] = {
    val edge = new Edge[T]()
    symbol match {
      case s: EndSymbol => {
        edge.append(symbol)
        this.endSymbolEdges.append(edge)
        edge.parent = this
        edge
      }
      case _ => {
        edge.append(symbol)
        edge.parent = this
        children.put(symbol.symbol,edge)
        edge
      }
    }
  }

  def addChild(edge: Edge[T]): Unit = {
    edge.parent = this
    edge.getLabel().size match {
      case 0 => endSymbolEdges.append(edge)
      case _ => { children.put(edge.getLabel()(0),edge)
          parentSuffixCount = edge.suffixCount + edge.parent.parentSuffixCount
      }
    }
  }

  def getNextEdge(currentEdge: Edge[T]): Edge[T] = {

    val it = children.iterator
    var nextEdge: Edge[T] = null
    breakable {
      while (it.hasNext) {
        var tuple = it.next()
        if (tuple._2 == currentEdge) {
          if ( {
            tuple = it.next
            tuple != null
          }) {
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
