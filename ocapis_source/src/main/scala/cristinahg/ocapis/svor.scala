package cristinahg.ocapis

abstract class svor{
  val C:Double=0.1
  val K:Double=0.1

  def train(traindat: Array[Array[Double]],trainLabels: Array[Int],params:Array[Double]=Array(C,K)):Array[Array[Double]]
  def test(trainPatterns:Array[Array[Double]],testPatterns:Array[Array[Double]],kernelType:String,
           kernelP:Array[Double],projection:Array[Double],thres:Array[Double]):Array[Array[Double]]
}
