package one.niu.outcome.gst.traits

import one.niu.outcome.gst.{ActiveNode, Edge, EndSymbol, Node}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by ericfalk on 22/11/2016.
  */
trait BaseGeneralSuffixTree[T] {

  //Pull some of this elements up to the base trait
  protected val root = new Node[T](null)
  protected val activeNode = new ActiveNode[T](root)

  protected var remainingSuffixCount = 0
  protected var lastCreatedNode: Node[T] = null

  protected val currentWord: mutable.Buffer[T] = ListBuffer.empty
  protected val currentEdges: mutable.Buffer[Edge[T]] = ListBuffer.empty
  protected val currentEndSymbols: mutable.Buffer[EndSymbol] = ListBuffer.empty

  protected val words: mutable.HashMap[String,T] = mutable.HashMap.empty

}
