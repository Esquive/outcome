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

    //TODO: Implement a generic symbol foreach logic, so the user specifies the logic on how to iterate over the suffixes.
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

          this.activeNode.activeEdge = this.getActiveEdgeOrCreate(currentSymbol)
          break

        } else {

          val edge = this.activeNode.activeEdge

          if (edge.suffixCount > 0 && edge.suffixCount <= this.activeNode.activeLength) {

            if (edge.child != null) {
              this.activeNode.activeNode = edge.child
              this.activeNode.activeLength = 0
              this.activeNode.activeEdge = this.getActiveEdgeOrCreate(currentSymbol)
              break
            } else {
//              //It is an EndSymbol Edge move along the suffix link
//              this.activeNode.activeNode = this.activeNode.activeNode.suffixLink
//              this.activeNode.activeEdge = this.getActiveEdgeOrCreate(currentSymbol)
              throw new IllegalStateException()
            }

          } else {

            if (edge.equalSymbolAt(currentSymbol, this.activeNode.activeLength)) {
              this.activeNode.activeLength += 1
              break
            } else {
              val node = this.splitEdge(edge, currentSymbol)
              this.handleSuffixLink(node)
            }

          }

        } //End If activeLength

      } //End While

    } //End Breakable

  }

  /**
    *
    * @param edge
    * @param currentSymbol
    * @return
    */
  private def splitEdge(edge: Edge[T], currentSymbol: Symbol[T]): Node[T] = {

    //TODO: Handle the currentSymbol.symbol situtation
    //Split/Create the new edges
    val edge1 = edge.sliceEdge(this.activeNode.activeLength, edge.suffixCount)

    //TODO: Change the code for the endsymbol handling
    val edge2 = currentSymbol match {
      case s: EndSymbol => {
        val edge = new Edge[T]; edge.append(currentSymbol); edge
      }
      case _ => new Edge[T](ListBuffer[T](currentSymbol.symbol))
    }
    //    edge1.parent = this.activeNode.activeNode
    //    edge2.parent = this.activeNode.activeNode

    //Create an internal Node
    //TODO: Change the code to handle EndSymbols: I need to purge/ move the endSymbols when splitting
    edge.child = new Node[T]()
    edge.child.addChild(edge1)
    edge.child.addChild(edge2)
    edge.child.suffixLink = root

    //Add the split edges to the current edge update and remove the initital one
    this.currentEdges.remove(this.currentEdges.indexOf(edge))

    //TODO: See if the code works if I have the edge label update before.
    //edge.setLabel(edge.getLabel().slice(0, this.activeNode.activeLength))
    edge.child.parentSuffixCount = edge.suffixCount + edge.parent.parentSuffixCount

    this.currentEdges.append(edge1)
    this.currentEdges.append(edge2)

    if (this.activeNode.activeNode == this.root) {
      this.activeNode.activeLength -= 1
      this.activeNode.activeEdge = this.activeNode.activeNode.getNextEdge(this.activeNode.activeEdge)
    } else {
      this.activeNode.activeNode = activeNode.activeNode.suffixLink
      this.activeNode.activeEdge = this.getActiveEdgeOrCreate(currentSymbol)
      //TODO: Get the next active edge
    }

    this.remainingSuffixCount -= 1
    edge.child
  }

  /**
    *
    * @param node
    */
  private def handleSuffixLink(node: Node[T]): Unit = {
    if (lastCreatedNode == null) {
      this.lastCreatedNode = node
    } else {
      this.lastCreatedNode.suffixLink = node
      this.lastCreatedNode = node
    }
  }


  /**
    * The active node is querid for the edge for a given symbol. If the edge does not exist it is created.
    * @param currentSymbol
    * @return
    */
  private def getActiveEdgeOrCreate(currentSymbol: Symbol[T]): Edge[T] = {

    val edge = this.activeNode.activeNode.getChild(currentSymbol)
    if (edge == null) {

      //If it is not existing we create it. And we add it to the currentEdges.
      (this.activeNode.activeNode, currentSymbol) match {
        case (n, s) if (n == this.root && s.isInstanceOf[EndSymbol]) => {
          //Do nothing:  The endSymbol is explicit for every Suffix inserted in the tree.
          this.remainingSuffixCount -= 1
        }
        case _ => {
          val newEdge = this.activeNode.activeNode.addChild(currentSymbol)
          currentEdges.append(newEdge)
          this.remainingSuffixCount -= 1
          return newEdge
        }
      }
    } else {
      this.activeNode.activeEdge = edge
      this.activeNode.activeLength += 1
      return edge
    }
    return edge
  }


  /***
    * Reset the variables used to insert an element in the suffix tree.
    */
  private def reset(): Unit = {
    this.activeNode.activeNode = this.root
    this.remainingSuffixCount = 0
    this.lastCreatedNode = null
    this.currentEdges.clear()
    this.currentWord.clear()
  }

}


