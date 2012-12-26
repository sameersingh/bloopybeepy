package edu.umass.cs.iesl.bp

import cc.factorie.{Variable, DiscreteVariable}
import cc.factorie.la.DenseTensor1

/**
 * @author sameer
 * @date 9/7/11
 */

object BPUtil {

  def message[V <: DiscreteVariable](v: V, scores: Array[Double]): GenericMessage = new DiscreteMessage(v, new DenseTensor1(scores))

  def uniformMessage: GenericMessage = UniformMessage

  def deterministicMessage[V <: Variable](v: V, value: Any): GenericMessage =
    value match {
      case valoo: v.Value => new DeterministicMessage[v.Value](valoo)
    }
}