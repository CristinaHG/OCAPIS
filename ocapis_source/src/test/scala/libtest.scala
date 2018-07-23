package cristinahg.ocapis
import scala.collection.mutable.ArrayBuffer
import org.scalatest._

class libtest extends FunSuite{
  val labels=Array(1,1,2,1,3,2,1)
  val m1=Array(Array(1.0,2.0),Array(3.0,4.0))
  val m2=Array(Array(3.0,4.0),Array(5.0,6.0))
  val gaussianK=Array(3.3546262790251185E-4, 0.1353352832366127, 1.522997974471263E-8, 3.3546262790251185E-4)
  val linearK=Array(9.0,13.0,11.0,16.0)
  val polynomialK=Array(9.5, 13.5,11.5,16.5)
  val sigmoidK=Array(1,1,1,1)
  val trainDat=io.Source.fromFile(getClass.getResource("/balance0train").getPath)

  // each row is an array of strings (the columns in the csv file)
  var rows = ArrayBuffer[Array[Double]]()
  //val readed =trainDat.getLines().map(s => s.split(" ").map(_.toDouble))

  for (line <- trainDat.getLines) {
    var item=line.split(" ").map(_.trim.toDouble)
//    val doubled=item(0).map(a=>a.toDouble)
    rows+=item
//    rows:+item
  }

  val casted=rows.toArray
  val labs=Array(1,2,3,1,1,2)
  print(casted(0)(0))
//  test("length of computed weights should equal labels size"){
//    assert(svmop.computeWeights(1,labels).length==labels.length)
//  }
//
//  test("Array from kernel matrix of type gaussian should equal gaussianK"){
//    assert(kdlor.computeKernelMatrix(m1, m2, "gaussian", Array(1.0)).toArray===gaussianK)
//  }
//
//  test("Check if elements order in gaussian kernel matrix are correct"){
//    val matrix=kdlor.computeKernelMatrix(m1, m2, "gaussian", Array(1.0))
//    assert(matrix(0,0)==3.3546262790251185E-4)
//    assert(matrix(0,1)==1.522997974471263E-8)
//    assert(matrix(1,0)==0.1353352832366127)
//    assert(matrix(1,1)==3.3546262790251185E-4)
//  }
//
//  test("Array from kernel matrix of type linear should equal linealK"){
//    assert(kdlor.computeKernelMatrix(m1, m2, "linear", Array(1.0)).toArray===linearK)
//  }
//
//  test("Check if elements order in linear kernel matrix are correct"){
//    val matrix=kdlor.computeKernelMatrix(m1, m2, "linear", Array(1.0))
//    assert(matrix(0,0)==9.0)
//    assert(matrix(0,1)==11.0)
//    assert(matrix(1,0)==13.0)
//    assert(matrix(1,1)==16.0)
//  }
//
//  test("Array from kernel matrix of type polynomial should equal polynomialK"){
//    assert(kdlor.computeKernelMatrix(m1, m2, "poly", Array(1.0)).toArray===polynomialK)
//  }
//
//  test("Check if elements order in polynomial kernel matrix are correct"){
//    val matrix=kdlor.computeKernelMatrix(m1, m2, "poly", Array(1.0))
//    assert(matrix(0,0)==9.5)
//    assert(matrix(0,1)==11.5)
//    assert(matrix(1,0)==13.5)
//    assert(matrix(1,1)==16.5)
//  }
//
//  test("Array from kernel matrix of type sigmoid should equal sigmoidK"){
//    assert(kdlor.computeKernelMatrix(m1, m2, "sigmoid", Array(1.0,2.0)).toArray===sigmoidK)
//  }
//
//  test("Check if elements order in sigmoidal kernel matrix are correct"){
//    val matrix=kdlor.computeKernelMatrix(m1, m2,"sigmoid", Array(1.0,2.0))
//    assert(matrix(0,0)==1)
//    assert(matrix(0,1)==1)
//    assert(matrix(1,0)==1)
//    assert(matrix(1,1)==1)
//  }


}
