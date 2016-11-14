package one.niu.outcome.gst

import java.util.UUID

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

/**
  * Created by ericfalk on 31/10/2016.
  */
class GeneralSuffixTree[T] extends GeneralSuffixTreeInterface[T] {

  protected val root = new Node[T]
  protected val activeNode = new ActiveNode[T](root)

  protected var remainingSuffixCount = 0
  protected var lastCreatedNode: Node[T] = null

  protected val currentWord: mutable.Buffer[T] = ListBuffer.empty[T]
  //TODO: Use a better structure for currentEdges
  protected val currentEdges: mutable.Buffer[Edge[T]] = ListBuffer.empty[Edge[T]]

  override def insert(word: mutable.Iterable[T]): Unit = {

    //TODO: Implement a generic symbol foreah logic, so the user specifies the logic on how to iterate over the suffixes.
    this.currentWord.appendAll(word)
    for (i <- 0 until word.size) {
      updateGst(new Symbol[T](currentWord(i)))
    }

    ///Insert the endSymbol for the String
    updateGst(new EndSymbol(UUID.randomUUID().toString).asInstanceOf[Symbol[T]])

    //Reset the parameters for the next word to insert in the tree
    reset()

  }

  protected def updateGst(currentSymbol: Symbol[T]): Unit = {

    //We have one suffix to add
    this.remainingSuffixCount += 1
    //Update the end of the path
    if (!this.currentEdges.isEmpty) {
      this.currentEdges.foreach(_.append(currentSymbol))
    }

    breakable {
      while (this.remainingSuffixCount > 0) {

        if (this.activeNode.activeLength == 0) {

          //TODO: Handle the currentSymbol.symbol situation
          //Check if the edge exists

          //TODO: Change the signature of this method: We must handle the endsymbol situation
          val edge = this.activeNode.activeNode.children.getOrElse(currentSymbol.symbol, null)
          if (edge == null) {

            //If it is not existing we create it. And we add it to the currentEdges
            //TODO: Change the signature of this method: We must handle the endsymbol situation
            this.activeNode.activeNode.children.put(currentSymbol.symbol, {
              val edge = new Edge[T](ListBuffer(currentSymbol.symbol))
              edge.parent = this.activeNode.activeNode
              this.currentEdges.append(edge)
              edge
            })

            this.remainingSuffixCount -= 1

            //Exit the loop
            break

          } else {

            this.activeNode.activeEdge = edge
            this.activeNode.activeLength += 1
            break

          }

        } else {

          val edge = this.activeNode.activeEdge

          if (edge.suffixCount <= this.activeNode.activeLength) {
            if (edge.child != null) {
              this.activeNode.activeNode = edge.child
              this.activeNode.activeLength = 0
              break
            } else {
              throw new IllegalStateException()
            }
          }

          if (edge.equalSymbolAt(currentSymbol,this.activeNode.activeLength)) {
            this.activeNode.activeLength += 1
            break()
          } else {
            val node = this.splitEdge(edge, currentSymbol)
            this.handleSuffixLink(node)
          }

        } //End If activeLength
      } //End While
    } //End Breakable


  }

  private def splitEdge(edge: Edge[T], currentSymbol: Symbol[T]): Node[T] = {

    //TODO: Handle the currentSymbol.symbol situtation
    //Split/Create the new edges
    val edge1 = new Edge[T](edge.getLabel().slice(this.activeNode.activeLength, edge.getLabel().size))
    val edge2 = new Edge[T](ListBuffer[T](currentSymbol.symbol))
    edge1.parent = this.activeNode.activeNode
    edge2.parent = this.activeNode.activeNode

    //Create an internal Node
    edge.child = new Node[T](mutable.LinkedHashMap[T, Edge[T]](edge1.getLabel()(0) -> edge1, edge2.getLabel()(0) -> edge2))
    edge.child.suffixLink = root


    //Add the split edges to the current edge update and remove the initital one
    this.currentEdges.remove(this.currentEdges.indexOf(edge))

    //TODO: See if the code works if I have the edge label update before.
    edge.setLabel(edge.getLabel().slice(0, this.activeNode.activeLength))
    edge.child.parentSuffixCount = {
      if(edge.parent != root) {
        edge.suffixCount + edge.parent.parentSuffixCount
      } else {
        edge.suffixCount
      }
    }

    this.currentEdges.append(edge1)
    this.currentEdges.append(edge2)

    if (this.activeNode.activeNode == this.root) {
      this.activeNode.activeLength -= 1
      this.activeNode.activeEdge = this.activeNode.activeNode.getNextEdge(this.activeNode.activeEdge)
    } else {
      this.activeNode.activeNode = activeNode.activeNode.suffixLink
    }

    this.remainingSuffixCount -= 1
    edge.child
  }

  private def handleSuffixLink(node: Node[T]): Unit = {
    if (lastCreatedNode == null) {
      this.lastCreatedNode = node
    } else {
      this.lastCreatedNode.suffixLink = node
      this.lastCreatedNode = node
    }
  }

  private def reset(): Unit = {
    this.activeNode.activeNode = this.root
    this.remainingSuffixCount = 0
    this.lastCreatedNode = null
    this.currentEdges.clear()
    this.currentWord.clear()
  }

}


