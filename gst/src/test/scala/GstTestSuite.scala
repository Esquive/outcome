import one.niu.outcome.gst.GeneralSuffixTree
import one.niu.outcome.gst.traits.InsertableGeneralSuffixTree
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
  * Created by ericfalk on 09/11/2016.
  */
class GstTestSuite extends JUnitSuite {

//  @Before def initialize(): Unit = {
//    gst = new GeneralSuffixTree[Char]
//  }

  @Test def gstSingleStringBuild(): Unit = {
    var gst: InsertableGeneralSuffixTree[Char] = null
    gst = new GeneralSuffixTree[Char]
    gst.insert("xyzxyaxyz".toCharArray)
  }

  @Test def gstSingleIntArrayBuild(): Unit = {
    var gst: InsertableGeneralSuffixTree[Int] = null
    gst = new GeneralSuffixTree[Int]
    gst.insert(Array(1,2,3,1,2,24,1,2,3))
  }


}
