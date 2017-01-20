import one.niu.outcome.dag.{NominalDAG, Sequence}
import one.niu.outcome.dag.traits.InsertableNominalDAG
import org.junit.Test
import org.scalatest.junit.JUnitSuite

/**
  * Created by ericfalk on 16/01/2017.
  */
class DAGTestSuite extends JUnitSuite{

  @Test def dagSingleStringBuild(): Unit = {

    var dag: InsertableNominalDAG[String] = null
    dag = new NominalDAG[String]()

    var sequence = new Sequence[String]("Hello;World;!", (x:String) => {
      val sequ = x
      val strArrays = x.split(";")
      val byteArrays = strArrays.map(_.getBytes)
      byteArrays.toIterable
    })

    val first = dag.insert(sequence)
    val second = dag.insert({sequence.sequence="Hello;World2;!"; sequence})
    val third = dag.insert({sequence.sequence="Hello;World3;?"; sequence})
    val fourth = dag.insert({sequence.sequence="Hello;World:!"; sequence})
    val fitht = dag.insert({sequence.sequence="Hello;World2;!"; sequence})

    println("done")


  }


}
