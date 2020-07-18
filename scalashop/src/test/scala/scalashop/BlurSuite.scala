package scalashop

import java.util.concurrent._
import scala.collection._
import org.junit._
import org.junit.Assert.assertEquals

class BlurSuite {
  trait testImgOne {

    val el1: RGBA = rgba(15,23,54,6) // 0, 0
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

  trait testImgTwo {

    val el1: RGBA = rgba(1, 2, 3, 4) // 0, 0
    val el2: RGBA = rgba(1, 2, 3, 4)
    val el3: RGBA = rgba(1, 2, 3, 4)
    val el4: RGBA = rgba(1, 2, 3, 4)
    val el5: RGBA = rgba(1, 2, 3, 4)  // 1, 1
    val el6: RGBA = rgba(1, 2, 3, 4)
    val el7: RGBA = rgba(1, 2, 3, 4)
    val el8: RGBA = rgba(1, 2, 3, 4)
    val el9: RGBA = rgba(1, 2, 3, 4)

    val image = new Img(3, 3, Array[RGBA](el1, el2, el3, el4, el5, el6, el7, el8, el9))
  }

  trait testImgThree {
    //3*4

    val el1: RGBA = rgba(15,23,54,6) // 0, 0
    val el2: RGBA = rgba(2,53,94,200)
    val el3: RGBA = rgba(1,12,94,180)
    val el4: RGBA = rgba(69,23,34,123)
    val el5: RGBA = rgba(15,23,4,20) // 1, 1
    val el6: RGBA = rgba(5,6,7,8)
    val el7: RGBA = rgba(180,190,20,200)
    val el8: RGBA = rgba(34,53,52,1)
    val el9: RGBA = rgba(15,23,54,6)
    val el10: RGBA = rgba(2,53,94,200)
    val el11: RGBA = rgba(1,12,94,180)
    val el12: RGBA = rgba(69,23,34,123)

    val image = new Img(3, 4, Array[RGBA](el1, el2, el3, el4, el5, el6, el7, el8, el9, el10, el11, el12))
  }




  @Test def `test boxBlurKernel`: Unit =
    new testImgOne {

      val blurred = boxBlurKernel(image, 1, 1, 1)
      val avgOne = (15 + 2 + 1 + 69 + 15 + 5 + 180 + 34 + 15) / 9
      val avgTwo = (23 + 53 + 12 + 23 + 23 + 6 + 190 + 53 + 23) / 9
      val avgThree = (54 + 94 + 94 + 34 + 4 + 7 + 20 + 52 + 54) / 9
      val avgFour = (6 + 200 + 180 + 123 + 20 + 8 + 200 + 1 + 6) / 9

      assertEquals(rgba(avgOne, avgTwo, avgThree, avgFour), blurred)
    }
  @Test def `test boxBlurKernel with oversized radius`: Unit =
    new testImgOne {

      val blurred = boxBlurKernel(image, 1, 1, 3)
      val avgOne = (15 + 2 + 1 + 69 + 15 + 5 + 180 + 34 + 15) / 9
      val avgTwo = (23 + 53 + 12 + 23 + 23 + 6 + 190 + 53 + 23) / 9
      val avgThree = (54 + 94 + 94 + 34 + 4 + 7 + 20 + 52 + 54) / 9
      val avgFour = (6 + 200 + 180 + 123 + 20 + 8 + 200 + 1 + 6) / 9

      assertEquals(rgba(avgOne, avgTwo, avgThree, avgFour), blurred)
    }

  @Test def `test boxBlurKernel does not leak`: Unit =
    new testImgTwo {
      val blurred = boxBlurKernel(image, 0, 0, 1)

      assertEquals(image.apply(0,0), blurred)
    }

  @Test def `test boxBlurKernel at edge`: Unit =
    new testImgOne {
      val blurred = boxBlurKernel(image, 0, 0, 1)

      val avgOne = (15 + 2 + 69 + 15) / 4
      val avgTwo = (23 + 53 + 23 + 23) / 4
      val avgThree = (54 + 94 + 34 + 4) / 4
      val avgFour = (6 + 200 + 123 + 20) / 4

      assertEquals(rgba(avgOne, avgTwo, avgThree, avgFour), blurred)
    }

  @Test def `test boxBlurKernel returns identity`: Unit =
    new testImgOne {
      val blurred = boxBlurKernel(image, 0, 0, 0)

      assertEquals(image.apply(0, 0), blurred)
    }



  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
