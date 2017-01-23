package one.niu.outcome.dag

import java.nio.ByteBuffer
import scala.collection.mutable
import one.niu.outcome.dag.traits.{BaseNominalDAG, InsertableNominalDAG}

/**
  * Created by ericfalk on 16/01/2017.
  */
@SerialVersionUID(1234L)
class NominalDAG[T] extends BaseNominalDAG[T] with InsertableNominalDAG[T]
                            with Serializable {

  val levels = mutable.ListBuffer.empty[mutable.HashMap[ByteBuffer, Node]]
  val leafs = mutable.HashMap.empty[ByteBuffer, Node]

  //TODO: This will have to be thread safe
  override def insert(sequence: Sequence[T]): Array[Byte] = {

    //Init variables
    var currentNode = rootNode

    var createdPathNumber: ByteBuffer = null

    var usedPathNumbers: mutable.Set[ByteBuffer] = mutable.Set.empty[ByteBuffer]
    val newPathNumberWrapper = ByteBuffer.wrap(this.getBytesForCurrentPathNumber)
    val symbols = sequence.getSymbols().toIterator

    //Backlog of traversed nodes
    val nodeBacklog = mutable.Stack[Node]()


    var counter = -1
    while (symbols.hasNext) {

      counter += 1

      //Get the next symbol
      val symbol = symbols.next()
      val symbolWrapper = ByteBuffer.wrap(symbol)

      val symbolString = new String(symbol)

      //Check if the current node has a corresponding children
      if (currentNode.children.contains(symbolWrapper)) {

        //Replace the current currentNode with the child already contained in the list
        val node = currentNode.children.getOrElse(symbolWrapper, null)

        //Get the used path information (only if it is less generic than the current)
        if (node.level > 0) {
          val temp = node.keyDictionary.getOrElse(currentNode.symbol, null)
          if (usedPathNumbers.size == 0) {
            usedPathNumbers = temp
          }
          else {
            //TODO: Check the case of empty intersection
            usedPathNumbers = temp.intersect(usedPathNumbers)
          }
        } else {
          if(this.levels.size == 1){
            //This case should be avoided since this means only one feature is used. And the childrens are unique
            usedPathNumbers = mutable.Set(node.children.toList(0)._2.symbol)
          }
        }

        currentNode = node
        //Add the traversed node to the backlog
        nodeBacklog.push(currentNode)
      }

      //If the node has no corresponding child we check if the structure of the lower level has the node.
      else if (levels.size > currentNode.level + 1 && levels(currentNode.level + 1).contains(symbolWrapper)) {

        //Get the node from the next level store and update the relationships
        val node = levels(currentNode.level + 1).getOrElse(symbolWrapper, null)

        //Now we have to create a new path
        node.parents.put(newPathNumberWrapper, currentNode)
        currentNode.children.put(symbolWrapper, node)
        createdPathNumber = newPathNumberWrapper

        currentNode = node
        nodeBacklog.push(currentNode)

      }
      //Otherwise the node does not exist
      else {

        //Check if the current level exists
        if (levels.size <= currentNode.level + 1) {
          levels += mutable.HashMap.empty[ByteBuffer, Node]
        }

        //Create the node and update the relationships
        val node = this.createInternalNode(symbolWrapper, currentNode.level + 1)
        levels(currentNode.level + 1).put(symbolWrapper, node)

        if (currentNode != this.rootNode) {
          node.parents.put(newPathNumberWrapper, currentNode)
        }
        createdPathNumber = newPathNumberWrapper
        currentNode.children.put(symbolWrapper, node)
        currentNode = node

        //Push the current Node to the backlog
        nodeBacklog.push(currentNode)

      }
    }


    //If a path was created all the traversed nodes are updated
    if (createdPathNumber != null) {
      val iterator = nodeBacklog.iterator
      var child: Node = null
      if (iterator.hasNext) {
        child = iterator.next()
      }
      while (iterator.hasNext) {
        val parent = iterator.next()
        child.keyDictionary.addBinding(parent.symbol, createdPathNumber)

        child = parent
      }

      //Crete the leaf node
      val leafNode = this.createLeafNode(createdPathNumber)
      leafNode.parents.put(newPathNumberWrapper, currentNode)
      currentNode.children.put(newPathNumberWrapper, leafNode)
      leafs.put(newPathNumberWrapper, leafNode)

      //TODO: Carry over the number of bytes to the leaf.
      this.updateCurrentPathNumber;
      newPathNumberWrapper.rewind().array()
      newPathNumberWrapper.array()


    } else {

      //After traversing the graph the usedPath Set should contain only a single element
      if (usedPathNumbers.size == 1) {
        val elem = usedPathNumbers.toList(0)
        elem.rewind()
        elem.array()
      } else {
        throw new IllegalStateException("After traversing the graph the usedPathNumbers should be unique")
      }

    }

  }

  private def createInternalNode(symbol: ByteBuffer, level: Int): Node = {
    //TODO: Use the same bytebuffer as for the Key to save memory
    new Node(false, false, level, symbol)
  }

  private def createLeafNode(pathNumber: ByteBuffer): Node = {
    ///TODO: Use the same bytebuffer as for the Key to save memory
    new Node(false, true, -1,
      pathNumber)
  }


}
