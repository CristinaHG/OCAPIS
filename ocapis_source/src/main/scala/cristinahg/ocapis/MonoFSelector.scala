package cristinahg.ocapis

import breeze.linalg.functions.minkowskiDistance
import breeze.linalg.{*, DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.numerics.constants._
import breeze.stats.{mean, median, stddev}
import breeze.numerics.exp

class MonoFSelector {

  private def computeRelation(k: Int, xiVal:Double, xjVal: Double):
  Double={
    1/(1+ exp(k*(xiVal-xjVal)))
  }

}
