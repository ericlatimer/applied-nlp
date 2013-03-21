package appliednlp.classify

import nak.core.AttrVal
import chalk.lang.eng.PorterStemmer


/**
 * An object that sets up the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object PpaFeaturesOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
For usage see below:
	     """)
    val help = opt[Boolean]("help", noshort = true, descr = "Show this message")
    val verbose = opt[Boolean]("verbose")
    val bitstringsSource = opt[String]("bitstrings", descr = "File containing bitstrings")
    val extendedFeatures = opt[Boolean]("extended",short='e', descr="Use extended features.")
    val inputFile = trailArg[String]("inputfile", descr = "Input file to create features from.")
  }
}


/**
 * An application for extracting features from the PPA native format for 
 * classification.
 */
object PpaFeatures {

  /**
   * The main method -- do the work. Don't change it.
   */
  def main(args: Array[String]) {

    // Parse and get the command-line options
    val opts = PpaFeaturesOpts(args)
   
    val inputFile = opts.inputFile()

    val bitstrings = opts.bitstringsSource.get match {
      case Some(bitstringsSource) =>
        io.Source.fromFile(bitstringsSource).getLines.map { line =>
          val Array(word, bitstring) = line.split("\\s+")
          (word -> BitVector(bitstring))
        }.toMap

      case None => new collection.immutable.HashMap[String, BitVector]()
    }

    val featureExtractor =
      if (opts.extendedFeatures()) new ExtendedFeatureExtractor(bitstrings)
      else BasicFeatureExtractor

    io.Source.fromFile(inputFile).getLines.foreach { line =>
      val Array(id, verb, noun, prep, prepObj, attach) = line.split(" ")
      val features = featureExtractor(verb, noun, prep, prepObj)
      println(features.map(_.toString).mkString(",") + "," + attach)
    }

  }

}

/**
 * A trait for classes that can extract features from the information in
 * the PPA files.
 */
trait FeatureExtractor {
  
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
object BasicFeatureExtractor extends FeatureExtractor {

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
    List(
      AttrVal("verb", verb),
      AttrVal("noun", noun),
      AttrVal("prep", prep),
      AttrVal("prep_obj", prepObj))
  }

}

/**
 * An extended feature extractor. It is your job to fill this out further.
 */
class ExtendedFeatureExtractor(bitvectors: Map[String, BitVector])
  extends FeatureExtractor {

  lazy val stemmer = new PorterStemmer

  override def apply(
    verb: String, noun: String, prep: String, prepObj: String): Iterable[AttrVal] = {
 
    // Use the basic feature extractor to get the basic features (no need to 
    // duplicate effort and specify it again).
    val basicFeatures = BasicFeatureExtractor(verb, noun, prep, prepObj)

    // Extract more features
    val verbNoun = AttrVal("verb+noun",verb + "+" + noun)
    val verbStem = AttrVal("verbStem",stemmer(verb))
    val nounForm = AttrVal("nounForm",getNounForm(noun))

	// Maybe mod the bitvector by something for a feature...
	// example bitvector: 00000000000111101110100100011001
	val verbBV: BitVector = bitvectors(verb)
	val nounBV: BitVector = bitvectors(noun)
	val prepBV: BitVector = bitvectors(prep)
	val prepObjBV: BitVector = bitvectors(prepObj)

	val verbT6  = AttrVal("verbT6",verbBV.keepTopBits(6).toString)
	val nounT6 = AttrVal("nounT6",nounBV.keepTopBits(6).toString)
	val prepT6 = AttrVal("prepT6",prepBV.keepTopBits(6).toString)
	val prepObjT6 = AttrVal("prepobjT6",prepObjBV.keepTopBits(6).toString)

	val verb6 = AttrVal("verb6",verbBV.getBottomBits(6).toString)
	val noun6 = AttrVal("noun6",nounBV.getBottomBits(6).toString)
	val prep6 = AttrVal("prep6",prepBV.getBottomBits(6).toString)
	val prepObj6 = AttrVal("prepObj6",prepObjBV.getBottomBits(6).toString)

	val firstSixBits = List(verbT6,nounT6,prepT6,prepObjT6)
	val lastSixBits = List(verb6,noun6,prep6,prepObj6)

    val extendedFeatures = List(verbNoun,verbStem,nounForm)
	
    // Return the features. You should of course add your features to basic ones.
     basicFeatures ++ firstSixBits ++ lastSixBits ++ extendedFeatures 
  }

  def getVerbStem(verb: String): String = {
    val verbLC = verb.toLowerCase
    if (verbLC.endsWith("ing")) verb.substring(0,verb.length-3) else
    if (verbLC.endsWith("es") || verbLC.endsWith("ed")) verb.substring(0,verb.length-2) else
	verb
  }

  def getNounForm(noun: String): String = {
	val NumRE = """[/d]*[,.]*[/d]+""".r
	val AllCapsRE = """[A-Z]+""".r 
    if (NumRE.pattern.matcher(noun).matches) "number" else
	if (noun.charAt(0).isUpper) "Xx" else
	if (AllCapsRE.pattern.matcher(noun).matches) "XX" else
	"null"
  }
}

/**
 * This is an entirely cruddy, slow implementation of a bit vector,
 * not using any bitwise ops, etc., but it should suffice for this problem.
 *
 * And, yes, we are using Ints where it could be Booleans, and we could have
 * the wrong values in there, but this keeps it easy, and again, is sufficient
 * for this problem.
 * 
 * Feel free to add more capability to this if it helps you create better
 * features.
 */
class BitVector(bits: IndexedSeq[Int]) {

  /**
   * Get the bit value at the given index.
   */
  def apply(index: Int) = bits(index)

  /**
   * Get the integer value of the bits
   */
  lazy val toInt = Integer.parseInt(bits.mkString, 2)

  /**
   *  Keep the top bits up to the given index, and then make the remaining bits
   *  zero.
   */
  def keepTopBits(index: Int) =
    new BitVector(bits.take(index) ++ Vector.fill(bits.length - index)(0))

  /**
   *  Keep the bottom bits up to the given index, and then make the remaining bits
   *  zero.
   */
  def keepBottomBits(index: Int) =
    new BitVector(Vector.fill(bits.length - index)(0) ++ bits.reverse.take(index) )

  /**
   *  Just get the bottom bits up to a given index
   */
  def getBottomBits(index: Int) =
    new BitVector(bits.takeRight(index) )

  /**
   * Concatenate the bits together.
   */
  override def toString = bits.mkString
}

/**
 * Companion object to the BitVector class.
 */
object BitVector {

  /**
   * Create a bit vector from a string of zeros and ones.
   */
  def apply(bitstring: String) =
    new BitVector(bitstring.split("").drop(1).map(_.toInt).toIndexedSeq)
}



