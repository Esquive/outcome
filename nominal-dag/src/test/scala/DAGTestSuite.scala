import java.io._
import java.lang.Exception
import java.lang.instrument.Instrumentation
import java.nio.ByteBuffer

import one.niu.outcome.dag.{NominalDAG, Sequence}
import one.niu.outcome.dag.traits.{InsertableNominalDAG, ReadableNominalDAG}
import org.apache.commons.io.IOUtils
import org.junit.Test
import org.scalatest.junit.JUnitSuite


/**
  * Created by ericfalk on 16/01/2017.
  */
class DAGTestSuite extends JUnitSuite {

  @Test def dagSingleStringBuild(): Unit = {

    val outputFile = new File("files/year_latest_noheader.csv.oc")
    if(outputFile.exists()) outputFile.delete()
    var dag: InsertableNominalDAG[String] = null
    dag = new NominalDAG[String]()

    var sequence = new Sequence[String]("nothing", (x: String) => {
      val sequ = x
      val strArrays = x.split(",")
      val byteArrays = strArrays.map(_.getBytes)
      byteArrays.toIterable
    },
      (x: Iterable[ByteBuffer], size: Int) => {
        var offset = 0
        val result = new Array[Byte](size + x.size - 1)
        x.foreach(buffer => {
          buffer.rewind()
          buffer.get(result, offset, buffer.capacity())
          if((offset + buffer.capacity()) < result.length) result(offset + buffer.capacity()) = ",".getBytes()(0)
          offset += buffer.capacity() + 1
        }
        )
        new String(result)
      }
    )


    //Compression
    val reader = new BufferedReader(new FileReader(new File("files/year_latest_noheader_100000.csv")))
    val writer = new BufferedWriter(new FileWriter(new File("files/year_latest_noheader.csv.oc")))
    var line: String = null
    var code: Array[Byte] = null
    var unequal = 0

    //compression
    var i = 0
    while ( {
      line = reader.readLine();
      line != null
    }) {
      //      println(i)
      i += 1
      code = dag.insert({
        sequence.sequence = line; sequence
      })
      try{
      val decomp = dag.asInstanceOf[ReadableNominalDAG[String]].read(ByteBuffer.wrap(code))
      val initialSequence = sequence.getSequence(decomp._1, decomp._2)
      if(!initialSequence.equals(line)){
        unequal += 1
      }} catch {
        case e: Exception => {
          e.printStackTrace()
        }
      }
      val length = (code.length & 0xFFF).toByte
      writer.write(length)
      writer.write(new String(code))
    }


    IOUtils.closeQuietly(reader)
    writer.flush()
    IOUtils.closeQuietly(writer)

    println("Number of faulty decompressions: " + unequal)

//    decompression
//    val input = new BufferedInputStream(new FileInputStream(new File("files/year_latest_noheader.csv.oc")))
//    var buffer = new Array[Byte](512)
//    var remaining : Array[Byte] = null
//    var readOffset = 0
//    while({
//      readOffset = input.read(buffer,0,buffer.length)
//      readOffset != -1
//    }){
//      var offset = 0
//      while(offset < buffer.length){
//        val size = buffer(offset)
//        buffer.slice(offset,)
//
//        val encoded =
//      }
//    }






    //    val first = dag.insert(sequence)
    //    val second = dag.insert({sequence.sequence="Hello;World2;!"; sequence})
    //    val third = dag.insert({sequence.sequence="Hello;World3;?"; sequence})
    //    val fourth = dag.insert({sequence.sequence="Hello;World;!"; sequence})
    //    val fitht = dag.insert({sequence.sequence="Hello;World2;!"; sequence})
    //    val sixth = dag.insert({sequence.sequence="Hello2;World2;!"; sequence})
    //    val seventh = dag.insert({sequence.sequence="Hello;World;?"; sequence})
    //    val eigth = dag.insert({sequence.sequence="Hello2;World;?"; sequence})
    //    val ninth = dag.insert({sequence.sequence="Hello;World;?"; sequence})
    //    val tenth = dag.insert({sequence.sequence="Hello2;World;?"; sequence})


    //    val out = new ObjectOutputStream(new FileOutputStream("encoding.ocenc"))
    //    out.writeObject(dag)
    //    out.flush()
    //    out.close()



  }


}
