package cristinahg.ocapis

import org.scalatest._

class svmoptest extends FunSuite{
  val svm=new svmop()
  var labels=Array(1,1,2,1,3,2,1)

  test("length of computed weights should equal labels size"){
    assert(svm.computeWeights(1,labels).length==labels.length)
  }
}
