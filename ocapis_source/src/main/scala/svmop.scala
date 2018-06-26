package cristinahg.ocapis

private class svmop(val cost:Double=0.1,val gamma:Double=0.1, var weights:Boolean=true){
  var svmweights=Array[Double]()

  //def fitsvm()

  def computeWeights(p:Int,targets:Array[Int]):Array[Double]={
    targets.foreach(i=> if(i<=p) svmweights.update(targets.indexOf(i),p+1-targets(targets.indexOf(i))) * targets.filter(_<=p).length / )
  }
}
object svmop{

  //  def givenumber:Int=
  //  return 12
}