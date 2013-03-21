package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer
import chalk.util.SimpleTokenizer

/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object TopicFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val inputDir = trailArg[String]("inputdir", descr = "Input dir to create features from.")
  }
}


/**
 * An application for extracting features from the Topic native format for 
 * classification.
 */
object TopicFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = TopicFeaturesOpts(args)
   
    val inputDir = opts.inputDir()

    val featureExtractor2 = BasicFeatureExtractor2

	for (file <- new java.io.File(inputDir).listFiles) {
		val tokens = SimpleTokenizer(io.Source.fromFile(inputDir+"/"+file.getName()).getLines.mkString)
		val filteredTokens = tokens.filter(x => x.length > 3 )
								   .groupBy(x => x)
								   .mapValues(_.length)
								   .filter(_._2 > 2)
								   .toList //.map{case (x,y) => (x,y)}
								   .sortBy(_._2)
								   .reverse
								   .take(10)
								   
		println(filteredTokens)				   
		//filteredTokens.foreach(println)
	}

	/*
    io.Source.fromFile(inputDir).getLines.foreach { line =>
      val Array(id, verb, noun, prep, prepObj, attach) = line.split(" ")
      val features = featureExtractor2(verb, noun, prep, prepObj)
      println(features.map(_.toString).mkString(",") + "," + attach)
    }
	*/
  }

}

/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait FeatureExtractor2 {
  
  /**
   * Given the verb, noun, preposition, and prepositional object,
   * create a set of AttrVal objects. (A "feature" is an attribute with a
   * value.) 
   */
  def apply(verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal]
}

/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicFeatureExtractor2 extends FeatureExtractor2 {

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
    List(
      AttrVal("verb", verb),
      AttrVal("noun", noun),
      AttrVal("prep", prep),
      AttrVal("prep_obj", prepObj))
  }

}

