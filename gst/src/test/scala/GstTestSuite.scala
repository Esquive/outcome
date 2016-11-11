import one.niu.outcome.gst.{GeneralSuffixTree, GeneralSuffixTreeInterface}
import org.junit.{Before, Test}
import org.scalatest.junit.JUnitSuite

/**
  * Created by ericfalk on 09/11/2016.
  */
class GstTestSuite extends JUnitSuite {

  var gst: GeneralSuffixTreeInterface[Char] = null

  @Before def initialize(): Unit = {
    gst = new GeneralSuffixTree[Char]
  }

  @Test def gstSingleStringBuild(): Unit = {
    gst.insert("xyzxyaxyz".toCharArray)
  }


}
