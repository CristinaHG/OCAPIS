// package cristinahg.ocapis

class svmop(val cost:Double=1.0,val gamma:Double=0.1, var weights:Boolean=true){
  var svmweights=Array[Double]()

  //def fitsvm()

  def computeWeights(p:Int,targets:Array[Int]):Array[Double]={
    //targets.foreach(i=> if(i<=p) svmweights.update(targets.indexOf(i),p+1-targets(targets.indexOf(i))) * targets.filter(_<=p).length / )
    val weightscomputed=targets.map(i=>
          if(i<=p) (p + 1 - i*1.0) * (targets.filter(_ <= p).length ) / (targets.filter(_ <= p).map(j=> p+1-j).sum)
          else (i*1.0-p) * (targets.filter(_> p).length) / (targets.filter(_> p).map(j=>j-p).sum)
        )
    return weightscomputed
  }
}
object svmop{

  def main(args: Array[String]): Unit = {
    var svm=new svmop()
    var labels=Array(1,1,2,1,3)
    //labels.foreach(i=>print(i+"\n"))
    val weightsnuevos=svm.computeWeights(1,labels)
    //weightsnuevos.foreach(i=>print(i+", "))

  }
  //  def givenumber:Int=
  //  return 12
}