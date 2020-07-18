package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  trait testImg {
    /* terrain for level 1*/

    val el1: RGBA = rgba(15,23,54,6)
    val el2: RGBA = rgba(2,53,94,200)
    val el3: RGBA = rgba(1,12,94,180)
    val el4: RGBA = rgba(69,23,34,123)
    val el5: RGBA = rgba(15,23,4,20) // 1, 1
    val el6: RGBA = rgba(5,6,7,8)
    val el7: RGBA = rgba(180,190,20,200)
    val el8: RGBA = rgba(34,53,52,1)
    val el9: RGBA = rgba(15,23,54,6)

    val image = new Img(3, 3, Array[RGBA](el1, el2, el3, el4, el5, el6, el7, el8, el9))
  }

  @Test def `test blurFilter`: Unit =
    new testImg {
      val blurred = boxBlurKernel(image, 1, 1, 1)
      val avgOne = (15 + 2 + 1 + 69 + 5 + 180 + 34 + 15) / 8
      val avgTwo = (23 + 53 + 12 + 23 + 6 + 190 + 53 + 23) / 8
      val avgThree = (54 + 94 + 94 + 34 + 7 + 20 + 52 + 54) / 8
      val avgFour = (6 + 200 + 180 + 123 + 8 + 200 + 1 + 6) / 8

      assertEquals(rgba(avgOne, avgTwo, avgThree, avgFour), blurred)
    }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
