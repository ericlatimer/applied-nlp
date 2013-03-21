package appliednlp.classify

/**
 * An application that takes a gold labeled file and a file containing
 * predictions, and then computes the accuracy for the top-third most
 * confident instances and the accuracy for the bottom-third (least 
 * confident instances).
 */
object ConfidenceScorer {

  def main(args: Array[String]) {
	val predictions = scala.io.Source.fromFile(args(1)).getLines.toIndexedSeq
	val testData = scala.io.Source.fromFile(args(0)).getLines.toList
	val correctPredictions = for (line <- testData) yield {
		val features = line.split(",").toList
		features(features.length -1)
	}

	val predTuples = for (p <- predictions) yield {
		val split = p.split(" ")
		val correct = correctPredictions(predictions.indexOf(p))
		(split(0), split(1), split(2), split(3), correct)
	}
	

	val sortedTuples = predTuples.sortBy(_._2)
	val correctPreds = for (st <- sortedTuples) yield if (st._1 == st._5) true else false

	val third = sortedTuples.length/3
	val thirds = correctPreds.grouped(third).toList
	
	val high = if (thirds.length > 3) thirds(2) ++ thirds(3) else thirds(2)
	val highCorrect = high.filter(x => x == true).length
	val midCorrect = thirds(1).filter(x => x == true).length
	val lowCorrect = thirds(0).filter(x => x == true).length

	println("High confidence accuracy: " + highCorrect.toDouble/high.length*100)
	println("Mid confidence accuracy: " + midCorrect.toDouble/third*100)
	println("Low confidence accuracy: " + lowCorrect.toDouble/third*100)
  }  

}
