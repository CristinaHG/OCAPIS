package cristinahg.ocapis

class svmop(val cost:Double=1.0,val gamma:Double=0.1, var weights:Boolean=true){
  var svmweights=Array[Double]()

  def computeWeights(p:Int,targets:Array[Int]):Array[Double]={
    val weightscomputed=targets.map(i=>
          if(i<=p) (p + 1 - i*1.0) * (targets.filter(_ <= p).length ) / (targets.filter(_ <= p).map(j=> p+1-j).sum)
          else (i*1.0-p) * (targets.filter(_> p).length) / (targets.filter(_> p).map(j=>j-p).sum))
    return weightscomputed
  }

}

  //  def givenumber:Int=
  //  return 12
