import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps
import scala.math

object FirstHalf {
  
  def main() = {
    
    //load stopwords
    val english_stopwords = readFile("test/english-stop.txt").split(" +").toList 
    
    //load pg11 and pg11-net
    val pg11 = readFile("test/pg11.txt")
    val pg11_net = readFile("test/pg11-net.txt")
    
    //load all the files that will be evaluated with cosinesim()
    val fileSet = new java.io.File("test").listFiles.filter((x) => x.getName.startsWith("pg") && x.getName.endsWith(".txt")).toSet
    
    
    
    /*
     * 							Evaluating freq() function
     */
    
    Statistics.topN(freq(pg11).sortWith(moreFrequent), 10)
    
    /*
     * 							Evaluating nonstopfreq() function
     */
    
    Statistics.topN(nonstopfreq(pg11, english_stopwords).sortWith(moreFrequent), 10)
    
    /*
     * 							Evaluating paraulesfreqfreq() function
     */
    
    Statistics.paraulafreqfreqStats(paraulafreqfreq(pg11).sortWith(_._1 < _._1), 10, 5)
    
     /*
     * 							Evaluating ngrams() function
     */
    
    Statistics.topN(ngramsfreq(pg11, 3).sortWith(moreFrequent), 10)
    
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
     * Given a list of tuples (_ Int), it prints the tuple in a fancy way and lists the division between _._2 and the total sum of _._2 of all the tuples on the list
     * In our case, we use this function to print the list of frequencies of certain words in a file, being _._2 the absolute count of occurrences of this word.
     * @param frequencyList List of tuples (_, Int)
     * @param n Number of elements to be shown
     */
    def topN(freqencyList: List[(_, Int)], n: Int) = {
      val nWords = freqencyList.foldLeft(0) { (total, actual) => total + actual._2 } 
      val nDiffWords = freqencyList length;
      println("N Words: " + nWords + " Diferent: " + nDiffWords)
      printf("%-30s %-11s %-10s\n", "Words", "ocurrences", "frequency")
      for(r <- freqencyList.slice(0,n)) printf("%-30s %-11d %-10.7f%%\n", r._1, r._2, (r._2.toFloat/nWords)*100) //println(r._1 + "			" + r._2 + "	" + (r._2.toFloat/nWords)*100)
      println()
    }
    
    /*	Given a frequency list "List[(Int, Int)]", a nMost and a nLeast number, it shows (in a fancy way) the first nMost and the last nLeast elements of the list
     * 	@param frequenctList The frequency list
     * 	@param nMost Number of top elements to show
     * 	@param nLeast Number of bottom elements to show
     */
    def paraulafreqfreqStats(frequencyList: List[(Int, Int)], nMost: Int, nLeast: Int) = {
      println("Les " + nMost + " frequencies mes frequents:")
      for(elem <- frequencyList.slice(0, nMost))
        println(elem._2 + " paraules apareixen " + elem._1)
      println("Les " + nLeast + " frequencies menys frequents:")
      for(elem <- frequencyList.slice(frequencyList.length-nLeast, frequencyList.length)) 
        println(elem._2 + " paraules apareixen " + elem._1 + " vegades")
      println()
    }
    
    /*	Given two strings (representing file names) and a number (representing its cosinesim), prints the explaination of how this three values correlate to each other
     * 	@param s1 First string
     * 	@param s2 Second string
     * 	@param cosinesim The number
     */
    def cosineSimStat(s1: String, s2: String, cosinesim: Double) = {
      printf("La similitud cosinus entre %s i %s es de %10.10f\n", s1, s2, cosinesim)
    }
  }
  
  /*	Given two tuples @p x and @p y, evaluates to true if the second element of the y is smaller than the second element of x. On draw, it reverses
   * 	the criteria and evaluates to true if the first element of x is smaller thant the first element of y.
   * 	@param x First tuple
   * 	@param y Second tuple
   */
  def moreFrequent(x: (String, Int), y: (String, Int)): Boolean = 
    x._2 > y._2 || (x._2 == y._2 && x._1 < y._1)
  
  /*	Given a character, evaluates to true if the character meets our criteria of acceptable character not to be filtered when reading a file.
   *	@param c The character to be evaluated
   */
  def acceptableChar(c: Char): Boolean = c.isLetter || c == ' '
  
  /*	Given a filename, produces a representative string that only contains lower case characters and spaces
   * 	@param filename the file path of the file to be readed
   * 	
   * 	@pre File must exist, otherwise will throw an exception
   */
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val str = try source.map(c => if(acceptableChar(c)) c else ' ').mkString finally source.close()
		str.toLowerCase.trim
  }
  
  /* Given a string @p s, evaluates as a list of absolute frequencies of the words (substrings spaced by white spaces ' ') that @p s contains.
   * More specifically, it evaluates as a list of pairs (String, Int), being the String a word of @p s and Int being its absolute frequency
   * @param s A string
   */
  def freq (s : String) = ngramsfreq(s, 1)
  
  /* Given a string @p s and a list of excluded words @stopwords, evaluates as the absolute frequencies of the words (substrings spaced by white spaces ' ') that @p s contains and @p stopwords does not
   * More specifically, it evaluates as a list of pairs (String, Int), being the String a word of @p s but not a word of @p stopwords, and Int being its absolute frequency
   * @param s A string
   * @param stopwords A list of strings
   */
  def nonstopfreq (s: String, stopwords: List[String]) = 
    freq(s).filterNot(x => stopwords.contains(x._1))
    
  /* 	Given a string representing a document, it computes the frequencies of the frequencies of its words
   * 	@param s String representing a document (string with white spaces)
   */
  def paraulafreqfreq(s: String): List[(Int, Int)] = {
    val freqfreqMap: collection.mutable.Map[Int, Int] = collection.mutable.Map() withDefaultValue(0)
    val frequencies = freq(s)
    for(wordFreq <- frequencies) freqfreqMap(wordFreq._2) += 1
    
    freqfreqMap.toList.sortWith((x,y) => x._2 > y._2 || (x._2 == y._2 && x._1 < y._1))
  }
  
  /*	Given a string representing a document (string with white spaces), it computes the frequency of all the ngrams of size n on that document
   * 	@param s String representing the document
   * 	@param n Length of the ngrams
   */
  def ngramsfreq(s: String, n: Int): List[(String,Int)] = {
    val wordMap: collection.mutable.Map[String, Int] = collection.mutable.Map() withDefaultValue(0);
    
    for ( i <- s.split(" +").sliding(n,1) ) wordMap(i.mkString(" ")) += 1 //counting words
    
    wordMap.toList
  }
  
  /*	Given two strings representing two filtered documents, and a list of words that will be filtered, it computes its cosine similarity
   * 	@param s1 First document
   * 	@param s2 Second document
   * 	@param stopwords List of words that will be discarded
   */
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
	
  //Aquesta funcio es canviarà
  def openPgTxtFiles(folder: String, startsWith: String, endsWith: String): Array[java.io.File] = {
    var fileList = new java.io.File(folder).listFiles.filter(_.getName.startsWith(startsWith)).filter(_.getName.endsWith(endsWith))
    fileList
  }
  
/* 
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
  
}*/

object MapReducer2 {
        
    /*def mappingTest3(filename: String, words: List[String]): List[(String, Int)] = {
      for(word <- words) yield (word,1234)
    }
  
    def reducingTest3(word: String, cosarara: List[Int]): List[Int] = {
      List(cosarara.length)
    }
    
    def loadfiles() = {
      val files = openPgTxtFiles("test")
      val input = for(file <- files) yield (file.getName, file)
      
      val system = ActorSystem("TextAnalizer2")
      val master = system.actorOf(Props( new MapReduceActor[String, List[java.io.File], String, Int](input.toList, mappingTest2, reducingTest2, 2, 2)))
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      println(result)
      
      system.shutdown
    }*/
  
    def start() = {
      val folder = "smalltest"
      
      val res1 = idf1(folder).asInstanceOf[Map[String,List[String]]] //Map[paraula, List[paths absoluts al fitxer que la contenen]]
      val res2 = idf2(res1).asInstanceOf[Map[String,List[Int]]] //Map[paraula, List[Ocurrències al conjunt de fitxers C]]
      //for(i <- res2) println(i._1 + " -> " + i._2.apply(0)) //els valors van de 0 a nDocuments
      
      val nDocuments = openPgTxtFiles(folder, "pg", ".txt").length.toDouble //Nombre de documents en el conjunt C
      val idfList = for(i <- res2) yield (i._1, math.log10(nDocuments/i._2.apply(0))) //calculem idf per cada paraula que apareix en el conjunt de documents
      //Segur que és el logaritme en base 10? Podria ser un map en comptes d'una list
      
      //for(idf <- idfList) println(idf._1 + " -> " + idf._2)
    }
    
    //--------------------- tf: Comptar num d'ocurrencies de cada paraula que su en UN document ---------------------
    
    def mappingCheapTf(filename: String, words: List[String]): List[(String, Int)] = {
      for(word <- words) yield (word,1234)
    }
  
    def reducingCheapTf(word: String, cosarara: List[Int]): List[Int] = {
      List(cosarara.length)
    }
    
    def CheapTf() = {
      
      val pg11 = FirstHalf.readFile("test/pg11.txt")   
      val input = List(("pg11.txt", pg11.split(" +").toList))
      
      val system = ActorSystem("TextAnalizer2")
      val master = system.actorOf(Props(new MapReduceActor[String, List[String], String, Int](input, mappingCheapTf, reducingCheapTf, 2, 2)))
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      system.shutdown
      
      println(result)
    }
    
    //--------------------- df1 ---------------------
    
    def mappingIdf1(file: String, words: List[String]): List[(String, String)] = {
      for(word <- words) yield (word, file)
    }
  
    def reducingIdf1(word: String, files: List[String]): List[String] = {
      files.distinct
    }
    
    def idf1(folder: String) = {
      val files = openPgTxtFiles(folder, "pg", ".txt")
      val input = for(file <- files) yield (file.getName, FirstHalf.readFile(file.getAbsolutePath).split(" +").toList)
      
      val system = ActorSystem("TextAnalizer2")
      val master = system.actorOf(Props(new MapReduceActor[String, List[String], String, String](input.toList, mappingIdf1, reducingIdf1, 2, 2)))
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      system.shutdown
      
      result //Map[string,List[string]] (per cada paraula, llista fitxers que la contenen) -------> podria ser una string amb el nom del fitxer
    }
    
    //--------------------- df2 ---------------------
    
    def mappingIdf2(word: String, files: List[String]): List[(String, Int)] = {
      List((word, files.length))
    }
  
    def reducingIdf2(word: String, files: List[Int]): List[Int] = {
      files
    }
    
    def idf2(lastResult: Map[String,List[String]]) = {

      val system = ActorSystem("TextAnalizer2")
      val master = system.actorOf(Props(new MapReduceActor[String, List[String], String, Int](lastResult.toList, mappingIdf2, reducingIdf2, 2, 2)))
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      system.shutdown
      
      result
    }
  }
	
  override def main(args:Array[String]) =  {
    //println(tractaxmldoc.readXMLFile("wiki-xml-2ww5k/32509.xml"))
    tractaxmldoc.exempleMateu
    //for(fitxer <- tractaxmldoc.openPgTxtFiles("test")) println(fitxer.getName)
    
    //FirstHalf.main()
    //val t1 = System.nanoTime
    //MapReducer2.start()
    //println("Temps: " + (System.nanoTime-t1)/Math.pow(10,9))
  }
}