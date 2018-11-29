import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps

object FirstHalf {
  
  def main() = {
    
    val english_stopwords = readFile("test/english-stop.txt").split(" +").toList
    
    val pg11 = readFile("test/pg11.txt")
    val pg11_net = readFile("test/pg11-net.txt")
    
    val fileSet = new java.io.File("test").listFiles.filter((x) => x.getName.startsWith("pg") && x.getName.endsWith(".txt")).toSet
    
    /*
     * 							Evaluating freq() function
     */
    
    Statistics.topN(freq(pg11).sortWith(lessFrequent), 10)
    
    /*
     * 							Evaluating nonstopfreq() function
     */
    
    Statistics.topN(nonstopfreq(pg11, english_stopwords).sortWith(lessFrequent), 10)
    
    /*
     * 							Evaluating paraulesfreqfreq() function
     */
    
    Statistics.paraulafreqfreqStats(paraulafreqfreq(pg11).sortWith(_._1 < _._1), 10, 5)
    
     /*
     * 							Evaluating ngrams() function
     */
    
    Statistics.topN(ngramsfreq(pg11, 3).sortWith(lessFrequent), 10)
    
    /*
     * 							Evaluating cosinesim() function
     */
    
    println(s"Similitud cosinus entre totes les permutacions dels fitxers: \n''${fileSet.mkString("'',''")}''")
    println("| ") 
    
    for(pair <- fileSet.subsets(2)){
      val t1 = System.nanoTime
      val file_1 = pair.head
      val file_2 = pair.tail.head
      val file_1_str = readFile(file_1.getAbsolutePath)
      val file_2_str = readFile(file_2.getAbsolutePath)
      print("| ")      
      Statistics.cosineSimStat(file_1.getName, file_2.getName, cosinesim(file_1_str, file_2_str, english_stopwords))
      println("|	Calculada en temps: " + (System.nanoTime-t1)/Math.pow(10,9))
    }
    
  }
  
  /*
   * This object contains the "fancy output" functions to evaluate the program results
   */
  object Statistics{
     
    /*
     * Given a list of tuples (_ Int), it prints the tuple in a fancy way and lists the division between _._2 and the total sum of _._2 of the list
     * In our case, we use this function to print the list of frequencies of certain words in a file, being _._2 the absolute count of occurrences of this word.
     * @param frequencyList List of tuples (_, Int)
     * @param 
     */
    def topN(freqencyList: List[(_, Int)], n: Int) = {
      val nWords = freqencyList.foldLeft(0) { (total, actual) => total + actual._2 } 
      val nDiffWords = freqencyList length;
      println("N Words: " + nWords + " Diferent: " + nDiffWords)
      printf("%-30s %-11s %-10s\n", "Words", "ocurrences", "frequency")
      for(r <- freqencyList.slice(0,n)) printf("%-30s %-11d %-10.7f%%\n", r._1, r._2, (r._2.toFloat/nWords)*100) //println(r._1 + "			" + r._2 + "	" + (r._2.toFloat/nWords)*100)
      println()
    }
    
    def paraulafreqfreqStats(frequencyList: List[(Int, Int)], nMost: Int, nLeast: Int) = {
      println("Les " + nMost + " frequencies mes frequents:")
      for(elem <- frequencyList.slice(0, nMost))
        println(elem._2 + " paraules apareixen " + elem._1)
      println("Les " + nLeast + " frequencies menys frequents:")
      for(elem <- frequencyList.slice(frequencyList.length-nLeast, frequencyList.length)) 
        println(elem._2 + " paraules apareixen " + elem._1 + " vegades")
      println()
    }
    
    def cosineSimStat(s1: String, s2: String, cosinesim: Double) = {
      printf("La similitud cosinus entre %s i %s es de %10.10f\n", s1, s2, cosinesim)
    }
  }
  
  /*	Given two tuples 
   * 	
   */
  def lessFrequent(x: (String, Int), y: (String, Int)): Boolean = 
    x._2 > y._2 || (x._2 == y._2 && x._1 < y._1)
  
  /*	Given a character, evaluates to true if the character meets our criteria of acceptable character not to be filtered when reading a file.
   *	@param c The character to be evaluated
   * 
   * 	@return true if c is a letter or a white space ' ', false otherwise
   */
  def acceptableChar(c: Char): Boolean = c.isLetter || c == ' '
  
  /*	Given a filename, produces a representative string that only contains lower case characters and spaces
   * 	@param filename the file path of the file to be readed
   * 	
   * 	@pre File must exist, otherwise will throw an exception
   * 	@return A representative string of the file only containing lower case letters and white spaces.
   */
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val str = try source.map(c => if(acceptableChar(c)) c else ' ').mkString finally source.close()
		str.toLowerCase.trim
  }
  
  /* Given a string @p s, returns the absolute frequencies of the words (substrings spaced by white spaces ' ') that @p s contains
   * @param s A string
   * 
   * @return A list of pairs (String, Int), being the String a word of @p s and Int being its absolute frequency
   */
  def freq (s : String) = ngramsfreq(s, 1)
  
  /* Given a string @p s and a list of excluded words @stopwords, returns the absolute frequencies of the words (substrings spaced by white spaces ' ') that @p s contains and @p stopwords does not
   * @param s A string
   * @param stopwords A list of strings
   * 
   * @return A list of pairs (String, Int), being the String a word of @p s but not a word of @p stopwords, and Int being its absolute frequency
   */
  def nonstopfreq (s: String, stopwords: List[String]) = 
    freq(s).filterNot(x => stopwords.contains(x._1))
    
  def paraulafreqfreq(s: String): List[(Int, Int)] = {
    val freqfreqMap: collection.mutable.Map[Int, Int] = collection.mutable.Map() withDefaultValue(0)
    val frequencies = freq(s)
    for(wordFreq <- frequencies) freqfreqMap(wordFreq._2) += 1
    
    freqfreqMap.toList.sortWith((x,y) => x._2 > y._2 || (x._2 == y._2 && x._1 < y._1))
  }
    
  def ngramsfreq(s: String, n: Int): List[(String,Int)] = {
    val wordMap: collection.mutable.Map[String, Int] = collection.mutable.Map() withDefaultValue(0);
    
    for ( i <- s.split(" +").sliding(n,1) ) wordMap(i.mkString(" ")) += 1 //counting words
    
    wordMap.toList
  }
  
  
  
  def cosinesim(s1: String, s2: String, stopwords: List[String]): Double = {
    val freq1 = nonstopfreq(s1, stopwords).toMap; 
    val freq2 = nonstopfreq(s2, stopwords).toMap;
    val freq1max = freq1.values.max
    val freq2max = freq2.values.max
    
    //normalizing vectors
    val freq1norm = freq1.map{ case (key, value) => (key, value.toDouble/freq1max) }
    val freq2norm = freq2.map{ case (key, value) => (key, value.toDouble/freq2max) }
    
    //aligning vectors
    val smv1 = freq2norm.map({ case (key, value) => (key, 0.0)}) ++ freq1norm
    val smv2 = freq1norm.map({ case (key, value) => (key, 0.0)}) ++ freq2norm
    
    //simplify vectors
    val smv1_vec = smv1.values.map(x => x.asInstanceOf[Double])
    val smv2_vec = smv2.values.map(x => x.asInstanceOf[Double])
    
    //compute cosinesim
    val term = smv1_vec.zip(smv2_vec).map(x => x._1 * x._2).reduceLeft( _ + _ )
    
    def euclidean_norm(v: Iterable[Double]) = {
      Math.sqrt( v.map( x => x*x ).reduceLeft( _ + _ ) )
    }
    
    term / (euclidean_norm(smv1_vec) * euclidean_norm(smv2_vec))
  }
    
}

object Main extends App {
  
	def compareAll(folderName: String): Any = {
    var fileList = new java.io.File(folderName).listFiles.filter(_.getName.endsWith(".xml"))
    for( i <- 0 to fileList.length -1 ){
      for( j <- i+1 to fileList.length -1 ){
        println(fileList.apply(i).getName + " -> " + fileList.apply(j).getName )
      }
    }
    
    "No se que retornara"
  }
  
def mappingTest(s: String, i: Int): List[(String, Int)] = {
  println("Estic mapejant " + s + " amb valor " + i)
  List((s,i))
}

def reducingTest(s: String, l: List[Int]): List[Int] = {
  println("Estic reduint " + s + " i " + l)
  l:::l:::l
}

object MapReducer {
  
  def textanalysis() = {
    
    var fileList = new java.io.File("test").listFiles.filter(_.getName.endsWith(".txt"))
    /*
    val system = ActorSystem("TextAnalizer")
    val master = system.actorOf(Props[MapReduceActor])
    master ! FileProcessing(fileList)*/
    
    val system = ActorSystem("TextAnalizer")
    
    val input = List(("hey", 1), ("wadup",2), ("not working", 3))
    val master = system.actorOf(Props( new MapReduceActor[String, Int, String, Int](input,mappingTest, reducingTest,2,2) ))
    
    implicit val timeout = Timeout(10 days)
    val futureResponse = master ? "start"
    val result = Await.result(futureResponse, Duration.Inf)
    println(result)
    
    system.shutdown
  }
  
}

def mappingTest2(filename: String, words: List[String]): List[(String, Int)] = {
    for(word <- words) yield (word,1)
  }

  def reducingTest2(word: String, cosarara: List[Int]): List[Int] = {
    //var dict = collection.mutable.Map[Int, Int] = collection.mutable.Map() withDefaultValue(0) //Map[String, List[Int]]() withDefault (k => List(0))
    //for((paraula, ocurrencies) <- l) dict += (paraula -> dict(paraula) + ocurrencies)
    List(cosarara.length)
  }

object MapReducer2 {
    
    def textanalysis2() = {
      
      val pg11 = FirstHalf.readFile("test/pg11.txt")   
      
      val input2 = List(("pg11.txt", pg11.split(" +").toList))
      
      val system = ActorSystem("TextAnalizer2")
      
      val master = system.actorOf(Props( new MapReduceActor[String, List[String], String, Int](input2, mappingTest2, reducingTest2, 2, 2) ))
      
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      
      println(result)
      
      system.shutdown
    }
  }

	
  override def main(args:Array[String]) =  {
    //println(tractaxmldoc.readXMLFile("wiki-xml-2ww5k/32509.xml"))
    //for(fitxer <- tractaxmldoc.openPgTxtFiles("test")) println(fitxer.getName)
    //MapReducer.textanalysis()
    //MapReducer2.textanalysis2()
    FirstHalf.main()
  }
}