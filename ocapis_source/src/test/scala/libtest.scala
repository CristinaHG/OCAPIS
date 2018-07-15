package cristinahg.ocapis

import org.scalatest._

class libtest extends FunSuite{
  val labels=Array(1,1,2,1,3,2,1)
  val m1=Array(Array(1.0,2.0),Array(3.0,4.0))
  val m2=Array(Array(3.0,4.0),Array(5.0,6.0))
  val gaussianK=Array(3.3546262790251185E-4, 0.1353352832366127, 1.522997974471263E-8, 3.3546262790251185E-4)
  //val polynomialK=Array()
  val linearK=Array(9.0,13.0,11.0,16.0)

  test("length of computed weights should equal labels size"){
    assert(svmop.computeWeights(1,labels).length==labels.length)
  }

  test("Array from kernel matrix of type gaussian should equal gaussianK"){
    assert(kdlor.computeKernelMatrix(m1, m2, "gaussian", Array(1.0)).toArray===gaussianK)
  }

  test("Array from kernel matrix of type linear should equal linealK"){
    assert(kdlor.computeKernelMatrix(m1, m2, "linear", Array(1.0)).toArray===linearK)
  }

}
