package neuralnetwork


import scala.collection.mutable.ListBuffer
import scala.util.Random

object NeuralNetwork extends App {

  case class Neuron(b_ws: Array[Double], activation: Double => Double) {
    def apply(input: List[Double]): Double =
      activation(
        input.zip(b_ws.tail).foldLeft(b_ws.head) { case (acc, (x, w)) =>
          acc + (x * w)
        }
      )
  }

  case class Layer(neurons: List[Neuron]) {
    def apply(input: List[Double]): List[Double] =
      neurons.map(_(input))
  }

  case class NeuralNetwork(layers: List[Layer]) {
    def apply(input: List[Double]): List[Double] =
      layers.foldLeft(input) { case (in, layer) => layer(in) }
  }

  class Param(a: Array[Double], idx: Int) {
    def value: Double = a(idx)

    def +=(v: Double): Unit = a(idx) += v
  }


  def builder(inputSize: Int, neuronsXlayer: List[Int]): (List[Param], NeuralNetwork) = {

    val params: ListBuffer[Array[Double]] = ListBuffer()

    def newArray(size: Int): Array[Double] = {
      val x = Array.fill(size + 1)(Random.between(-1.0, 1.0))
      params.addOne(x)
      x
    }

    val layers: List[Layer] =
      (inputSize :: neuronsXlayer)
        .sliding(2)
        .map { case List(in, out) =>
          Layer(
            List.fill(out)(Neuron(newArray(in), scala.math.tanh))
          )
        }
        .toList

    val p: List[Param] =
      params.map(a => (a.indices).map(i => new Param(a, i))).toList.flatten

    (p, NeuralNetwork(layers))

  }


  def loss(nn: List[Double] => List[Double], input: List[Double], expected: List[Double]): Double = {
    val predicted = nn(input)
    predicted.zip(expected).foldLeft(0.0) { case (acc, (p, e)) =>
      acc + Math.pow(p - e, 2)
    }
  }

  def `Df/Dp`(f: List[Double] => Double, input: List[Double], p: Param): Double = {
    val h: Double = 0.000001
    val y2 = f(input)
    p.+=(h)
    val y1 = f(input)
    p.+=(-h)
    (y1 - y2) / h
  }

  val (params, nn) = builder(3, List(4, 4, 1))

  val xs = List(
    List(2.0, 3.0, -1.0),
    List(3.0, -1.0, 0.5),
    List(0.5, 1.0, 1.0),
    List(1.0, 1.0, -1.0),
  )

  val ys = List(
    1.0,
    -1.0,
    -1.0,
    1.0
  )


  val learningRate = 0.01

  // training
  for (i <- 1 to 1000) {
    for ((x, y) <- xs.zip(ys)) {
      val score = loss(nn(_), x, List(y))
      println(s"$i   loss: $score, input: $x, target: $y, predicted: ${nn(x)}")

      for (p <- params) {
        val differentiableFun: List[Double] => Double = loss(nn(_), _, List(y))
        val g = `Df/Dp`(differentiableFun, x, p)
        p.+=( -learningRate * g)
      }
    }
  }




}

