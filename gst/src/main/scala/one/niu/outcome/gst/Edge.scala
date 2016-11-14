package one.niu.outcome.gst

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by ericfalk on 31/10/2016.
  */
class Edge[T](private var label : mutable.Iterable[T]) {


  //TODO: Define a default leaf node type?
  var parent : Node[T] = null
  var child : Node[T] = null

  var suffixCount = {label.size}
  var endSymbols = mutable.LinkedHashSet.empty[EndSymbol]

  def append(symbol: Symbol[T]): Unit = {

    if(symbol.isInstanceOf[EndSymbol]){

      val newSymbol = new EndSymbol(symbol.asInstanceOf[EndSymbol].symbol)
      newSymbol.index = {
        if(parent != null) {
          parent.parentSuffixCount + suffixCount
        } else {
          suffixCount
        }
      }
      endSymbols.add(newSymbol)
    } else {
      //Append a symbol to the label
      label.asInstanceOf[ListBuffer[T]] += symbol.symbol
      //Update the suffix count
      this.suffixCount = this.label.size
    }
  }

  def getSymbol(index: Int): Symbol[T] ={
    return new Symbol[T](label.asInstanceOf[ListBuffer[T]](index))
  }

  def equalSymbolAt(symbol: Symbol[T],index: Int): Boolean = {
    //TODO: Handle the symbol.symbol situation
    return {
      if(symbol.isInstanceOf[EndSymbol]){
        endSymbols.contains(symbol.asInstanceOf[EndSymbol])
      } else {
        label.asInstanceOf[ListBuffer[T]](index) == symbol.symbol
      }
    }
  }

  //TODO: create an update edge function to replace the two other functions
  def getLabel(): mutable.Buffer[T] = {
    return this.label.asInstanceOf[ListBuffer[T]]
  }

  def setLabel(newLabel : mutable.Buffer[T]) = {
    this.label = newLabel
    //Update the suffix count
    this.suffixCount = this.label.size
  }

}
