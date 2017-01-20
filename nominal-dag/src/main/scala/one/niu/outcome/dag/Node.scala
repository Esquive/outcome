package one.niu.outcome.dag

import java.nio.ByteBuffer
import scala.collection.mutable

/**
  * Created by ericfalk on 16/01/2017.
  */
class Node(val isRoot: Boolean = true, val isLeaf:Boolean = false, val level: Int, val symbol: Array[Byte]){

  val parents : mutable.HashMap[ByteBuffer,Node] = mutable.HashMap.empty[ByteBuffer,Node]
  val children : mutable.HashMap[ByteBuffer,Node] = mutable.HashMap.empty[ByteBuffer,Node]

  val keyDictionary : mutable.MultiMap[ByteBuffer,ByteBuffer] = new mutable.HashMap[ByteBuffer,Set[ByteBuffer]]() with mutable.MultiMap[ByteBuffer,ByteBuffer]

}
