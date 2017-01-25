package one.niu.outcome.dag.traits

import one.niu.outcome.dag.Node

/**
  * Created by ericfalk on 16/01/2017.
  */
trait BaseNominalDAG[T] {

  //The root node and the leaf nodes hold negative
  protected val rootNode = new Node(level = -1, symbol = null);

  private var currentPathNumber: BigInt = 0;

  protected def getBytesForCurrentPathNumber: Array[Byte] = {
    this.currentPathNumber.toByteArray
  }

  protected def updateCurrentPathNumber: Unit ={
    this.currentPathNumber += 1
  }

}
