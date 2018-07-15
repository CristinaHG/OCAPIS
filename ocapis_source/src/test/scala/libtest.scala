package cristinahg.ocapis

import org.scalatest._

class libtest extends FunSuite{
  val labels=Array(1,1,2,1,3,2,1)
  val m1=Array(Array(1.0,2.0),Array(3.0,4.0))
  val m2=Array(Array(3.0,4.0),Array(5.0,6.0))
  val gaussianK=Array(3.3546e-04,1.5230e-08,1.3534e-01,3.3546e-04)
  test("length of computed weights should equal labels size"){
    assert(svmop.computeWeights(1,labels).length==labels.length)
  }

  test("Array from kernel matrix of type gaussian should equal gaussianK"){
    assert(kdlor.computeKernelMatrix(m1,m2,"gaussian",Array(1.0)).toArray==gaussianK)
  }
}
