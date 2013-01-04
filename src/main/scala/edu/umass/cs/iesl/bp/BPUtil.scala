package edu.umass.cs.iesl.bp

import cc.factorie._
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

  def assignments1[N1 <: Variable](f1: Factor1[N1], varying: Set[Variable]): Iterator[Assignment] = {
    if (varying.size != 1 || varying.head != f1._1)
      throw new Error("Factor1.valuesIterator cannot vary arguments.")
    else
      f1._1.domain match {
        case d: DiscreteDomain => d.iterator.map(value => new Assignment1(f1._1, value.asInstanceOf[f1._1.Value]))
      }
  }

  def assignments2[N1 <: Variable, N2 <: Variable](f2: Factor2[N1, N2], varying: Set[Variable]): Iterator[Assignment] = {
    val values1: Iterable[DiscreteValue] = if (varying.contains(f2._1)) f2._1.domain.asInstanceOf[DiscreteDomain] else Seq(f2._1.value.asInstanceOf[DiscreteValue])
    val values2: Iterable[DiscreteValue] = if (varying.contains(f2._2)) f2._2.domain.asInstanceOf[DiscreteDomain] else Seq(f2._2.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2) yield new Assignment2(f2._1, val1.asInstanceOf[f2._1.Value], f2._2, val2.asInstanceOf[f2._2.Value])).iterator
  }

  def assignments3[N1 <: Variable, N2 <: Variable, N3 <: Variable](f3: Factor3[N1, N2, N3], varying: Set[Variable]): Iterator[Assignment] = {
    val values1 = if (varying.contains(f3._1)) f3._1.domain.asInstanceOf[DiscreteDomain] else Seq(f3._1.value.asInstanceOf[DiscreteValue])
    val values2 = if (varying.contains(f3._2)) f3._2.domain.asInstanceOf[DiscreteDomain] else Seq(f3._2.value.asInstanceOf[DiscreteValue])
    val values3 = if (varying.contains(f3._3)) f3._3.domain.asInstanceOf[DiscreteDomain] else Seq(f3._3.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2; val3 <- values3) yield new Assignment3(f3._1, val1.asInstanceOf[f3._1.Value], f3._2, val2.asInstanceOf[f3._2.Value], f3._3, val3.asInstanceOf[f3._3.Value])).iterator
  }

  def assignments4[N1 <: Variable, N2 <: Variable, N3 <: Variable, N4 <: Variable](f4: Factor4[N1, N2, N3, N4], varying: Set[Variable]): Iterator[Assignment] = {
    val values1 = if (varying.contains(f4._1)) f4._1.domain.asInstanceOf[DiscreteDomain] else Seq(f4._1.value.asInstanceOf[DiscreteValue])
    val values2 = if (varying.contains(f4._2)) f4._2.domain.asInstanceOf[DiscreteDomain] else Seq(f4._2.value.asInstanceOf[DiscreteValue])
    val values3 = if (varying.contains(f4._3)) f4._3.domain.asInstanceOf[DiscreteDomain] else Seq(f4._3.value.asInstanceOf[DiscreteValue])
    val values4 = if (varying.contains(f4._4)) f4._4.domain.asInstanceOf[DiscreteDomain] else Seq(f4._4.value.asInstanceOf[DiscreteValue])
    (for (val1 <- values1; val2 <- values2; val3 <- values3; val4 <- values4) yield new Assignment4(f4._1, val1.asInstanceOf[f4._1.Value], f4._2, val2.asInstanceOf[f4._2.Value], f4._3, val3.asInstanceOf[f4._3.Value], f4._4, val4.asInstanceOf[f4._4.Value])).iterator
  }

  def assignments(f: Factor, varying: Set[Variable]): Iterator[Assignment] = {
    f match {
      // Factor 1
      case f1: Factor1[_] => assignments1(f1, varying)
      // Factor 2
      case f2: Factor2[_, _] => assignments2(f2, varying)
      // Factor 3
      case f3: Factor3[_, _, _] => assignments3(f3, varying)
      // Factor 4
      case f4: Factor4[_, _, _, _] => assignments4(f4, varying)
    }
  }

}