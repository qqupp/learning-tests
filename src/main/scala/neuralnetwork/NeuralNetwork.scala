package neuralnetwork


import scala.collection.mutable.ListBuffer
import scala.util.Random

object NeuralNetwork extends App {

  class Param(var value: Double) {
    def +=(v: Double): Unit = value += v
  }

  case class Neuron(ws: List[Param], b: Param, activation: Double => Double) {
    def apply(input: List[Double]): Double =
      activation(
        input.zip(ws).foldLeft(b.value) { case (acc, (x, w)) =>
          acc + (x * w.value)
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


  def builder(inputSize: Int, neuronsXLayer: List[Int]): (List[Param], NeuralNetwork) = {

    val params: ListBuffer[Param] = ListBuffer()

    def newParams(size: Int): (List[Param], Param) = {
      val ws = List.fill(size)(new Param(Random.between(-1.0, 1.0)))
      val b = new Param(Random.between(-1.0, 1.0))
      params.addAll(ws)
      params.addOne(b)
      (ws, b)
    }

    val layers: List[Layer] =
      (inputSize :: neuronsXLayer)
        .sliding(2)
        .map { case List(in, out) =>
          Layer(
            List.fill(out) {
              val (ws, b) = newParams(in)
              Neuron(ws, b, scala.math.tanh)
            }
          )
        }
        .toList


    (params.toList, NeuralNetwork(layers))

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

  println(s"Parameters ${params.length}")

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
      val f: List[Double] => Double = loss(nn(_), _, List(y))

      val score = f(x)
      println(s"$i   loss: $score, input: $x, target: $y, predicted: ${nn(x)}")

      for (p <- params) {
        val g = `Df/Dp`(f, x, p)
        p.+=( -learningRate * g)
      }
    }
  }




}

