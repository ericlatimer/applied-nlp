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

	//println("nounBV31: " + nounBV)
	val verb31 = AttrVal("verb31",verbBV.getBottomBits(1).toString)
	val noun31 = AttrVal("noun31",nounBV.getBottomBits(1).toString)
	val prep31 = AttrVal("prep31",prepBV.getBottomBits(1).toString)
	val prepObj31 = AttrVal("prepObj31",prepObjBV.getBottomBits(1).toString)

	val verb3031 = AttrVal("verb3031",verbBV.getBottomBits(2).toString)
	val noun3031 = AttrVal("noun3031",nounBV.getBottomBits(2).toString)
	val prep3031 = AttrVal("prep3031",prepBV.getBottomBits(2).toString)
	val prepObj3031 = AttrVal("prepObj3031",prepObjBV.getBottomBits(2).toString)

	val verb293031 = AttrVal("verb293031",verbBV.getBottomBits(3).toString)
	val noun293031 = AttrVal("noun293031",nounBV.getBottomBits(3).toString)
	val prep293031 = AttrVal("prep293031",prepBV.getBottomBits(3).toString)
	val prepObj293031 = AttrVal("prepObj293031",prepObjBV.getBottomBits(3).toString)

	val verb28293031 = AttrVal("verb28293031",verbBV.getBottomBits(4).toString)
	val noun28293031 = AttrVal("noun28293031",nounBV.getBottomBits(4).toString)
	val prep28293031 = AttrVal("prep28293031",prepBV.getBottomBits(4).toString)
	val prepObj28293031 = AttrVal("prepObj28293031",prepObjBV.getBottomBits(4).toString)

	val verb2728293031 = AttrVal("verb2728293031",verbBV.getBottomBits(4).toString)
	val noun2728293031 = AttrVal("noun2728293031",nounBV.getBottomBits(4).toString)
	val prep2728293031 = AttrVal("prep2728293031",prepBV.getBottomBits(4).toString)
	val prepObj2728293031 = AttrVal("prepObj2728293031",prepObjBV.getBottomBits(4).toString)

	val lastFiveBits = List(verb2728293031,noun2728293031,prep2728293031,prepObj2728293031)
	val lastFourBits = List(verb28293031,noun28293031,prep28293031,prepObj28293031)
	val lastThreeBits = List(verb293031,noun293031,prep293031,prepObj293031)
	val lastTwoBits = List(verb3031,noun3031,prep3031,prepObj3031)
	val lastOneBit = List(verb31,noun31,prep31,prepObj31)
    val extendedFeatures = List(verbNoun,verbStem,nounForm)
	
    // Return the features. You should of course add your features to basic ones.
    basicFeatures ++ lastThreeBits ++ lastFourBits ++ extendedFeatures
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



