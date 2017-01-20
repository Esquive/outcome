package one.niu.outcome.dag

import java.nio.ByteBuffer
import scala.collection.mutable
import one.niu.outcome.dag.traits.{BaseNominalDAG, InsertableNominalDAG}

/**
  * Created by ericfalk on 16/01/2017.
  */
@SerialVersionUID(1234L)
class NominalDAG[T] extends BaseNominalDAG[T] with InsertableNominalDAG[T]
                                              with Serializable
{

  val levels = mutable.ListBuffer.empty[mutable.HashMap[ByteBuffer,Node]]
  val leafs = mutable.HashMap.empty[ByteBuffer,Node]

  //TODO: This will have to be thread safe
  override def insert(sequence: Sequence[T]): Array[Byte] = {

    //Init variables
    var currentNode = rootNode
    var currentLevel = -1
    var pathCreated = false
    var usedPathNumber: ByteBuffer = null
    val currentPathNumberWrapper = ByteBuffer.wrap(this.getBytesForCurrentPathNumber)
    val symbols = sequence.getSymbols().toIterator

    while(symbols.hasNext){

      //Get the next symbol
      val symbol = symbols.next()
      currentLevel += 1

      val symbolWrapper = ByteBuffer.wrap(symbol)

      //Check if the node has the correct children
      if(currentNode.children.contains(symbolWrapper)){

        //NO NEW PATH IS CREATED
        currentNode = currentNode.children.getOrElse(symbolWrapper,null)
//        usedPathNumber = currentNode.keyDictionary.getOrElse(symbolWrapper,null)

      } else if(levels.size > currentLevel && levels(currentLevel + 1).contains(symbolWrapper)) {
        //A NEW PATH IS CREATED

        //Get the node from the next level store and update the relationships
        val node = levels(currentLevel + 1).getOrElse(symbolWrapper,null)
        node.parents.put(currentPathNumberWrapper, currentNode)
        //Update the path information
        node.keyDictionary.put(symbolWrapper,currentPathNumberWrapper)
        usedPathNumber = currentPathNumberWrapper
        currentNode = node

        pathCreated = true

      } else {
        //A NEW PATH IS CREATED

        //Check if the current level exists
        if(levels.size <= currentLevel) {
          levels  += mutable.HashMap.empty[ByteBuffer,Node]
        }

        //Create the node and update the relationships
        val node = this.createInternalNode(symbol,currentLevel)
        levels(currentLevel).put(symbolWrapper,node)
        if(currentNode != this.rootNode) {
          node.parents.put(currentPathNumberWrapper, currentNode)
          node.keyDictionary.put(symbolWrapper,currentPathNumberWrapper)
          usedPathNumber = currentPathNumberWrapper
        }
        currentNode.children.put(symbolWrapper,node)
        currentNode = node

        pathCreated = true
      }

    }


    if(pathCreated) {
      val leafNode = this.createLeafNode()
      leafNode.parents.put(currentPathNumberWrapper, currentNode)
      currentNode.children.put(currentPathNumberWrapper, leafNode)
      leafs.put(currentPathNumberWrapper, leafNode)

      this.updateCurrentPathNumber;
      currentPathNumberWrapper.rewind().array()
      currentPathNumberWrapper.array()
    } else {
      //TODO: get the leaf node witht eh correct path number.
      usedPathNumber.rewind()
      usedPathNumber.array()
    }

  }

  private def createInternalNode(symbol: Array[Byte], level: Int): Node ={
    //TODO: Use the same bytebuffer as for the Key to save memory
    new Node(false,false,level,symbol)
  }

  private def createLeafNode(): Node = {
    ///TODO: Use the same bytebuffer as for the Key to save memory
    new Node(false,true,-1,
      this.getBytesForCurrentPathNumber)
  }


}
