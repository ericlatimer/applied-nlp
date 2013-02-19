package appliednlp.cluster

import nak.cluster._
import nak.cluster.Point
import nak.util.CollectionUtil._
import chalk.util.SimpleTokenizer

import org.apache.log4j.Logger
import org.apache.log4j.Level
import io.Source

/**
 *  Read data and produce data points and their features.
 *
 *  @param filename the name of the file containing the data
 *  @return a triple, the first element of which is a sequence of id's
 *     (unique for each row / data point), the second element is the sequence
 *     of (known) cluster labels, and the third of which is the sequence of
 *     Points to be clustered.
 */
trait PointCreator extends (String => Iterator[(String,String,Point)])

/**
 * Read data in the standard format for use with k-means.
 */
object DirectCreator extends PointCreator {

 def apply(filename: String) = {
   Source.fromFile(filename).getLines().map(line => {
     val splitLine = line.split(" ")
     (splitLine(0), splitLine(1), Point(IndexedSeq(splitLine(2).toDouble,splitLine(3).toDouble)))
   }).toIterator
 }

}


/**
 * A standalone object with a main method for converting the achieve.dat rows
 * into a format suitable for input to RunKmeans.
 */
object SchoolsCreator extends PointCreator {

  def apply(filename: String) = {
    Source.fromFile(filename).getLines().map(line => {
      val splitLine = line.split("    \\s*")
      List((splitLine(0).replaceAll(" ", "_")+"_4th", "4", Point(IndexedSeq(splitLine(1).toDouble,splitLine(2).toDouble))),
      (splitLine(0).replaceAll(" ", "_")+"_6th", "6", Point(IndexedSeq(splitLine(3).toDouble,splitLine(4).toDouble))))
    }).flatten.toIterator
  }

}

/**
 * A standalone object with a main method for converting the birth.dat rows
 * into a format suitable for input to RunKmeans.
 */
object CountriesCreator extends PointCreator {

  def apply(filename: String) = {
    Source.fromFile(filename).getLines().map(line => {
      val splitLine = line.split("\\s+")
      assert(splitLine.length == 3 || splitLine.length == 4)
      val country = if (splitLine.length == 4) splitLine(0)+"_"+splitLine(1) else splitLine(0)
      val point = if (splitLine.length == 4) Point(IndexedSeq(splitLine(2).toDouble,splitLine(3).toDouble)) else Point(IndexedSeq(splitLine(1).toDouble,splitLine(2).toDouble))

      (country, "1", point)
    }).toIterator
  }

}

/**
 * A class that converts the raw Federalist
 * papers into rows with a format suitable for input to Cluster. As part of
 * this, it must also perform feature extraction, converting the texts into
 * sets of values for each feature (such as a word count or relative
 * frequency).
 */
class FederalistCreator(simple: Boolean = false) extends PointCreator {

  def apply(filename: String) = {
    val articles = FederalistArticleExtractor(filename)
    val wordCountPoints = if (simple) extractSimple(articles.map{mp => mp("text")})
                              else extractFull(articles.map{mp => mp("text")})

    articles.map{mp =>
      (mp("id"),mp("author"),wordCountPoints(mp("id").toInt-1))
    }.toIterator
  }

  /**
   * Given the text of an article, compute the frequency of "the", "people"
   * and "which" and return a Point per article that has the frequency of
   * "the" as the value of the first dimension, the frequency of "people"
   * for the second, and the frequency of "which" for the third.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractSimple(texts: IndexedSeq[String]): IndexedSeq[nak.cluster.Point] = {
    getWordCounts(texts, List("the","people","which"))
  }

  /**
   * Helper function for extractSimple so that extractFull can utilize the same functionality (++)
   */
  def getWordCounts(texts: IndexedSeq[String], words: Seq[String], combine: Boolean = false): IndexedSeq[nak.cluster.Point] = {
    texts.map {t =>
      val counts = SimpleTokenizer(t)
        .groupBy(x => x.toLowerCase)
        .mapValues(_.length)
        .filterKeys(k => words.contains(k))

      if (!combine)
        Point(words.map{w => if (counts.contains(w)) counts(w).toDouble else 0.0}.toIndexedSeq)
      else
        Point(IndexedSeq(counts.values.sum.toDouble))
    }
  }

  /**
   * Given the text of an article, extract features as best you can to try to
   * get good alignment of the produced clusters with the known authors.
   *
   * @param texts A sequence of Strings, each of which is the text extracted
   *              for an article (i.e. the "text" field produced by
   *              FederalistArticleExtractor).
   */
  def extractFull(texts: IndexedSeq[String]): IndexedSeq[Point] = {
    val prepositions = List("of", "at", "in", "without", "between")
    val pronouns = List("he", "they", "anybody", "it", "one")
    val determiners = List("the", "a", "that", "my", "more", "much", "either", "neither")
    val conjunctions = List("and", "that", "when", "while", "although", "or")
    val auxiliary = List("be", "is", "am", "are", "have", "got", "do")
    val particles = List("no", "not", "nor", "as")
    val functionWords = prepositions ++ pronouns ++ determiners ++ conjunctions ++ auxiliary ++ particles

    //by functionWord groups counts
    val combine = true
    val prepCounts = getWordCounts(texts, prepositions, combine)
    val proCounts = getWordCounts(texts, pronouns, combine)
    val detCounts = getWordCounts(texts, determiners, combine)
    val conCounts = getWordCounts(texts, conjunctions, combine)
    val auxCounts = getWordCounts(texts, auxiliary, combine)
    val parCounts = getWordCounts(texts, particles, combine)
    val totCounts = getWordCounts(texts, functionWords, combine)

    //by functionWord group/ total word count ratio
    val functionWordAvg = texts.map{ t =>
      val tokens = SimpleTokenizer(t)
      val functionTokens = tokens.filter(x => functionWords.contains(x))

      Point(IndexedSeq(tokens.length/functionTokens.length.toDouble))
    }

    //average sentence length
    val avgSentenceLength = texts.map { t =>
      val sentenceEnders = Set(".", "!", "?")
      val tokens = SimpleTokenizer(t)
      val ends = tokens.filter(x => sentenceEnders.contains(x))

      Point(IndexedSeq(tokens.length/ends.length.toDouble))
    }

    //average semi colon use
    val semiColonFreq = texts.map { t =>
      val tokens = SimpleTokenizer(t)
      val semiColons = tokens.filter(x => x == ";")

      Point(IndexedSeq(tokens.length/semiColons.length.toDouble))
    }

    //average word length
    val avgWordLength = texts.map { t =>
      val tokens = SimpleTokenizer(t)
      val totalWordLength = tokens.map{ tk => tk.length}.sum

      Point(IndexedSeq(totalWordLength/tokens.length.toDouble))
    }

    //total word count
    val wordCounts = texts.map { t =>
      Point(IndexedSeq(SimpleTokenizer(t).length))
    }
    //println(wordCounts)

    //CAPITAL words count
    val capitalWordAvg = texts.map { t =>
      val AllCapsRE = "[A-Z]+".r

      val tokens = SimpleTokenizer(t)
      val allCaps = tokens.filter(x => AllCapsRE.pattern.matcher(x).matches)

      //println(allCaps)
      Point(IndexedSeq(allCaps.length/tokens.length.toDouble))
    }

    //CAPITAL word/total word count ratio

    val simple = getWordCounts(texts, List("the","people","which"))

    val allCounts = (parCounts zip auxCounts).map (ps => Point(ps._1.coord++ps._2.coord))

    val allCountsAndAvgs = (((allCounts zip functionWordAvg).map (ps => Point(ps._1.coord++ps._2.coord))
                              zip avgSentenceLength).map (ps => Point(ps._1.coord++ps._2.coord))
                              zip avgWordLength).map (ps => Point(ps._1.coord++ps._2.coord))

    val returnMe = (allCountsAndAvgs zip wordCounts).map (ps => Point(ps._1.coord++ps._2.coord))

    val returnMe2 = (wordCounts zip simple).map (ps => Point(ps._1.coord++ps._2.coord))
    //good: simple wordCounts
    returnMe2
  }

}

object FederalistArticleExtractor {
  /**
   * A method that takes the raw Federalist papers input and extracts each
   * article into a structured format.
   *
   * @param filename The filename containing the Federalist papers.
   * @return A sequence of Maps (one per article) from attributes (like
   *         "title", "id", and "text") to their values for each article.
   */
  def apply(filename: String): IndexedSeq[Map[String, String]] = {

    // Regex to identify the text portion of a document.
    val JustTextRE = (
      """(?s)\*\*\* START OF THIS PROJECT GUTENBERG.+""" +
      """\*\*\*(.+)\*\*\* END OF THIS PROJECT GUTENBERG""").r

    // Regex to capture different parts of each article.
    val ArticleRE = (
      """(?s)(\d+)\n+""" + // The article number.
      """(.+?)\n+""" + // The title (note non-greedy match).
      """((?:(?:For|From)[^\n]+?)?)\s+""" + // The publication venue (optional).
      """((?:(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday).+\d\d\d\d\.)?)\n+""" + // The date (optional).
      """((?:MAD|HAM|JAY).+?)\n+""" + // The author(s).
      """(To the [^\n]+)""" + // The addressee.
      """(.+)""" // The text.
      ).r

    val book = io.Source.fromFile(filename).mkString
    val text = JustTextRE.findAllIn(book).matchData.next.group(1)
    val rawArticles = text.split("FEDERALIST.? No. ")

    // Use the regular expression to parse the articles.
    val allArticles = rawArticles.flatMap {
      case ArticleRE(id, title, venue, date, author, addressee, text) =>
        Some(Map("id" -> id.trim,
          "title" -> title.replaceAll("\\n+", " ").trim,
          "venue" -> venue.replaceAll("\\n+", " ").trim,
          "date" -> date.replaceAll("\\n+", " ").trim,
          "author" -> author.replaceAll("\\n+", " ").trim,
          "addressee" -> addressee.trim,
          "text" -> text.trim))

      case _ => None
    }.toIndexedSeq

    // Get rid of article 71, which is a duplicate, and return the rest.
    allArticles.take(70) ++ allArticles.slice(71, allArticles.length)
  }

}
