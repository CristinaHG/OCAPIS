package cristinahg.ocapis

import java.io.{BufferedWriter, Closeable, FileOutputStream, OutputStreamWriter}

import scala.sys.process._

class svorex extends svor {
  val kerneltype="rbf"
  override def train(traindat: Array[Array[Double]], trainLabels: Array[Int], params: Array[Double]):
  Array[Array[Double]] ={


    using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream("svorex0train.csv")))) {
      writer =>
        for (x <- data) {
          writer.write(x + "\n")  // however you want to format it
        }
    io.Source.
    io.Source.fromFile(getClass.getResource("/predictions").getPath
  }
  }
  //override def test(trainPatterns: Array[Array[Double]], testPatterns: Array[Array[Double]], kernelP: Array[Double], projection: Array[Double], thres: Array[Double]): Array[Array[Double]] = ???


  def using[T <: Closeable, R](resource: T)(block: T => R): R = {
     try { block(resource) }
      finally { resource.close() }
  }

}