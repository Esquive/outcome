import java.io._

import one.niu.outcome.dag.{NominalDAG, Sequence}
import one.niu.outcome.dag.traits.InsertableNominalDAG
import org.apache.commons.io.IOUtils
import org.junit.Test
import org.scalatest.junit.JUnitSuite

/**
  * Created by ericfalk on 16/01/2017.
  */
class DAGTestSuite extends JUnitSuite{

  @Test def dagSingleStringBuild(): Unit = {

    var dag: InsertableNominalDAG[String] = null
    dag = new NominalDAG[String]()

    var sequence = new Sequence[String]("nothing", (x:String) => {
      val sequ = x
      val strArrays = x.split(",")
      val byteArrays = strArrays.map(_.getBytes)
      byteArrays.toIterable
    })

    val reader = new BufferedReader(new FileReader(new File("files/year_latest_noheader.csv")))
    val writer = new BufferedWriter(new FileWriter(new File("files/year_latest_noheader.csv.oc")))
    var line : String = null

    //compression
    var i = 0
    while({ line = reader.readLine();
      line != null
    }){
      println(i)
      i+=1
      val code = dag.insert({sequence.sequence=line; sequence})
      writer.write(new String(code))
      writer.newLine()
    }

    IOUtils.closeQuietly(reader)
    IOUtils.closeQuietly(writer)


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

    println("done")


  }


}
