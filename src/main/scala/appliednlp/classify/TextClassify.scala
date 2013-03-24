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
    val attach = opt[String]("a",descr="Attach for correct classification name")
    val stpwords = opt[String]("stop", descr="File of list of stopwords")
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
    val stopwords = io.Source.fromFile(opts.stpwords()).getLines.toSet
    val inputDir = opts.inputDir()
    val attach = opts.attach()

    val featureExtractor2 = BasicFeatureExtractor2
	
	for (file <- new java.io.File(inputDir).listFiles) {
		
		val tokens = SimpleTokenizer(io.Source.fromFile(inputDir+"/"+file.getName()).getLines.map(x => x + " ").mkString.toLowerCase)
		val filteredTokens = tokens.filter(_.exists(_.isLetter)).filterNot(x => stopwords.contains(x)).filter(x => x.length > 3 )

								   .groupBy(x => x)
								   .mapValues(_.length)
								   .filter(x => (x._2 > 2 || x._1.length > 4))
								   .toList //.map{case (x,y) => (x,y)}
								   .sortBy(_._2)
								   .reverse
								   .take(7)
								   
		//println(filteredTokens)	
		val features = featureExtractor2(filteredTokens)			   
		//filteredTokens.foreach(println)
		println(features.map(_.toString).mkString(",") + "," + attach)
	}
   
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
  //def apply(verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal]
   def apply(tokens: List[(String,Int)]): Iterable[AttrVal]	
}

/**
 * The simplest feature extractor: each word gets a feature, where the 
 * attribute is the type of the word. 
 */
object BasicFeatureExtractor2 extends FeatureExtractor2 {

  override def apply(
    tokens: List[(String,Int)]): Iterable[AttrVal] = {
      (for (token <- tokens) yield {
	AttrVal("word",token._1)
      }).toList

     
  }

}

