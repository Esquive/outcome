package one.niu.outcome.gst


import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

/**
  * Created by ericfalk on 31/10/2016.
  */
class GeneralSuffixTree[T] extends GeneralSuffixTreeInterface[T] {

  private val root = new Node[T]
  private val activeNode = new ActiveNode[T](root)

  //  private val end = new End(-1)
  private var remainingSuffixCount = 0
  private var lastCreatedNode: Node[T] = null

  private val currentWord: mutable.Buffer[T] = ListBuffer.empty[T]
  //TODO: Use a better structure for currentEdges
  private val currentEdges: mutable.Buffer[Edge[T]] = ListBuffer.empty[Edge[T]]

  override def insert(word: mutable.Iterable[T]): Unit = {

    //Reinitialize all the parameters
    this.activeNode.activeNode = root
    this.remainingSuffixCount = 0

    this.currentWord.appendAll(word)
    for (i <- 0 until word.size) {
      updateGst(i)
    }

    //TODO: Reset all parameters
    this.activeNode.activeNode = this.root
    this.remainingSuffixCount = 0
    this.lastCreatedNode = null
    this.currentEdges.clear()
    this.currentWord.clear()
  }

  private def updateGst(index: Int): Unit = {

    //We have one suffix to add
    this.remainingSuffixCount += 1
    //Update the end of the path
    if (!this.currentEdges.isEmpty) {
      this.currentEdges.foreach(_.append(currentWord(index)))
    }

    val currentSymbol = currentWord(index)

    breakable {

      while (this.remainingSuffixCount > 0) {

        if (this.activeNode.activeLength == 0) {

          //Check if the edge exists
          val edge = this.activeNode.activeNode.children.getOrElse(currentSymbol, null)
          if (edge == null) {

            //If it is not existing we create it. And we add it to the currentEdges
            this.activeNode.activeNode.children.put(currentSymbol, {
              val edge = new Edge[T](ListBuffer(currentSymbol))
              this.currentEdges.append(edge)
              edge
            })

            this.remainingSuffixCount -= 1

            //Exit the loop
            break

          } else {

            this.activeNode.activeEdge = Some(currentSymbol)
            this.activeNode.activeLength += 1
            break

          }

        } else {

          val edgePointer: T = this.activeNode.activeEdge.get
          val edge = {
            edgePointer match {
              case value => this.activeNode.activeNode.children.getOrElse(value, null)
              case null => throw new IllegalStateException()
            }
          }
          
          val edgeLabel = edge.getLabel()
          if (edgeLabel.size <= this.activeNode.activeLength) {
            if (edge.child != null) {
              this.activeNode.activeNode = edge.child
              this.activeNode.activeLength = 0
              break
            } else {
              throw new IllegalStateException()
            }

          }
          val edgeSymbol = edgeLabel(this.activeNode.activeLength)

          if (currentSymbol == edgeSymbol) {
            this.activeNode.activeLength += 1
            break()
          } else {


            val node = this.handleActiveEdge(edge, currentSymbol)
            if (lastCreatedNode == null) {
              this.lastCreatedNode = node
            } else {
              this.lastCreatedNode.suffixLink = node
              this.lastCreatedNode = node
            }


          }


        } //End If activeLength


      } //End While

    } //End Breakable


  }


  private def handleActiveEdge(edge: Edge[T], currentSymbol: T): Node[T] = {

    //Split the labels

    val edge1 = new Edge[T](edge.getLabel().slice(this.activeNode.activeLength, edge.getLabel().size))
    val edge2 = new Edge[T](ListBuffer[T](currentSymbol))
    //Create an internal Node
    //TODO: Handle the suffix Link
    edge.child = new Node[T](mutable.LinkedHashMap[T, Edge[T]](edge1.getLabel()(0) -> edge1, edge2.getLabel()(0) -> edge2))
    edge.child.suffixLink = root

    //Add the split edges to the current edge update and remove the initital one
    this.currentEdges.remove(this.currentEdges.indexOf(edge))

    //TODO: See if the code works if I have the edge label update before.
    edge.setLabel(edge.getLabel().slice(0, this.activeNode.activeLength))

    this.currentEdges.append(edge1)
    this.currentEdges.append(edge2)


    if (this.activeNode.activeNode == this.root) {

      this.activeNode.activeLength -= 1

      //Change the activeEdge
      val it = this.activeNode.activeNode.children.iterator


      //TODO: Find a better solution for this.
      breakable {
        while (it.hasNext) {
          var tuple = it.next()
          if (tuple._1 == this.activeNode.activeEdge.get) {
            if ((tuple = it.next) != null) {
              this.activeNode.activeEdge = Some(tuple._1)
              break
            }
            else {
              //TODO: Maybe we need to revisit here what if the iterator has no next Throw an Anomaly?
            }
          }
        }
      }
    } else {
      this.activeNode.activeNode = activeNode.activeNode.suffixLink
    }

    this.remainingSuffixCount -= 1

    edge.child
  }
}


