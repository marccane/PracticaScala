import akka.actor._
import akka.pattern.ask
import scala.concurrent._
import scala.concurrent.duration._
import akka.util.Timeout
import scala.language.postfixOps
import scala.math


/*	This object contains all the code of the first part of the hand in.
 */
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
    
    val t0 = System.nanoTime
    
    for(pair <- fileSet.subsets(2)){
      val t1 = System.nanoTime
      val file_1 = pair.head
      val file_2 = pair.tail.head
      val file_1_str = readFile(file_1.getAbsolutePath)
      val file_2_str = readFile(file_2.getAbsolutePath)
      print("| ")      
      Statistics.cosineSimStat(file_1.getName, file_2.getName, cosinesim(file_1_str, file_2_str, english_stopwords))
      println("|	Calculada en temps: " + (System.nanoTime-t1)/Math.pow(10,9) + " segons")
    }
    
    println("Temps total per calcular totes les comparacions: " + (System.nanoTime-t0)/Math.pow(10,9) + " segons")
    
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
  def moreFrequent[A <% Ordered[A],B <% Ordered[B]](x: (A, B), y: (A, B)): Boolean = 
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
  	val source = scala.io.Source.fromFile(filename, "UTF-8")
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
  
  /*	Computes the euclidean norm of a vector (of Double values)
   *	@param v the Vector
   */
  def euclidean_norm(v: Iterable[Double]) = {
    Math.sqrt( v.map( x => x*x ).reduceLeft( _ + _ ) )
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
    
    term / (euclidean_norm(smv1_vec) * euclidean_norm(smv2_vec))
  }
    
}


object Main extends App {
	
  /*	Given a folder and two strings with the prefix and sufix of the files to be opened it returns an array of file handlers
   * 	@param folder Name of the folder to search the files in
   * 	@param startsWith Prefix of the files to be opened
   * 	@param endsWith Sufix of the files to be opened
   */
  def openFiles(folder: String, startsWith: String, endsWith: String): Array[java.io.File] = {
    var fileList = new java.io.File(folder).listFiles.filter(_.getName.startsWith(startsWith)).filter(_.getName.endsWith(endsWith))
    fileList
  }
	
  object MapReduceRef {
  
    def mapping1(title: String, referencedDocs: List[String]): List[(String, String)] = {
      for( doc <- referencedDocs) yield (doc, title) //document, document que fa referencia al primer
    }
  
    def reducing1(title: String, docsRefer: List[String]): List[String] = {
      docsRefer.distinct
    }
    
    def mapping2(title: String, tuple: (List[String],List[String])): List[(String, String)] = {
      val allTitles = tuple._2
      for(titleRef <- allTitles; if(!tuple._1.contains(titleRef))) yield (title, titleRef) //allTitles.filterNot(x=>titlesRef.contains(x) || x.equals(title))
    }
  
    def reducing2(title: String, docsRefer: List[String]): List[String] = {
      docsRefer
    }
    
    def mapReduceDocumentsNoReferenciats() = {
      
      val files = Main.openFiles("100xml", "", ".xml").toList
      val input = for(file <- files) yield tractaxmldoc.referencies(file)
      val titles = tractaxmldoc.titols(files)
      
      var system = ActorSystem("DocsReferenciats")
      var master = system.actorOf(Props(new MapReduceActor[String, List[String], String, String](input, mapping1, reducing1, 2, 2)))
      implicit val timeout = Timeout(10 days)
      val futureResponse = master ? "start"
      val result = Await.result(futureResponse, timeout.duration)
      system.stop(master)
      
      val result1map = result.asInstanceOf[Map[String,List[String]]]
      val filterDocuments = result1map.filter(x => titles.contains(x._1))
      
      val retallat = filterDocuments.slice(0, 100)
      //for(i <- retallat) println(i._1 + " -> " + i._2)
      
      val result2senseMR = for(i <- filterDocuments.toList) yield (i._1,  titles.filterNot(x => i._2.contains(x) || x.equals(i._2)))//|| x.equals(title)
      print(result2senseMR.take(10))
      
      val input2 = for(elem <- filterDocuments.toList) yield {
        val temp = elem
        val title = temp._1
        val refs = temp._2
        (title, (refs, titles))
      }
      
      //print(input2.take(10))
      
      master = system.actorOf(Props(new MapReduceActor[String, (List[String], List[String]), String, String](input2, mapping2, reducing2, 2, 2)))
      val futureResponse2 = master ? "start"
      val result2 = Await.result(futureResponse, timeout.duration)
      system.shutdown
      
      val result2map = result2.asInstanceOf[Map[String,List[String]]]
      //println(result2map.take(5))
    }
  }

  override def main(args:Array[String]) =  {
    
    //FirstHalf.main()
    
    //MapReduceTfIdf.main1()
    
    MapReduceRef.mapReduceDocumentsNoReferenciats()
  }
}

object MapReduceTfIdf{
  
  /*	First mapping function. Given a file name (not the path) and a tuple (File_path, List[stopwords]) 
   * 	generates all the pairs (file name, (word, 1)). This will let us reduce the list to a list of ocurrences (frequency)
   * 	@param file_name The file name
   * 	@param file Tuple of (filePath, List[stopwords])
   */
  def mapping1(file_name: String, file: (String, List[String])): List[(String, (String, Double))] = {
    val wordList = tractaxmldoc.readXMLFile(file._1).split(" +").toList.filterNot(file._2.contains(_))
    
    val x = (for(word <- wordList) yield (file_name, (word, 1.toDouble)))
    x
  }
  
  //key-> Filename, values-> list of (Word, count)
  /* 	Given a file name and a list of tuples (Word, 1) (word occurrences) of that file, reduces the tuples with the same first value to a tuple (Word, n_ocurrences)
   * 	@param key Name of the file
   * 	@param values List of all occurrences of the words
   */
  def reducing1(key: String, values: List[(String, Double)]): List[(String, Double)] = {
    val res = for( (word, count_list) <- values.groupBy(_._1).toList ) 
      //For every pair of word and list of counts, add up its counts
      yield (word, count_list.map( {case (_, count) => count } ).reduceLeft( _ + _))
    
    val max_freq = res.map(_._2).max
    
    res.sortWith(FirstHalf.moreFrequent).map(x => (x._1, x._2/max_freq))
  }
  
  /*	Given a file name and its word counts, unfolds all its tuples to the reverse (Word, File) tuple. 
   * 	This will let us count how many documents contain that word and compute the idf of that word.
   * 	@param file_name Name of that file
   * 	@param word_count List of word counts (Word, 1)
   */
  def mapping2(file_name: String, word_count: List[(String, Double)]): List[(String, String)] = {
    word_count.map(x => (x._1, file_name))
  }
  
  /*	Given a file name, and a tuple containing the List of word counts and a Map with (at least) the idf of all words in that file
   * 	yields all the tuples (file name, (Word, tfidf)) for that list of word counts. This will let us compute the space model vector
   * 	of that file
   * 	@param file_name Name of the file
   * 	@param tf_idf_unfolded Pair containing the frequency counts of that file and the idf of all the words in that document set
   */
  def mapping3(file_name: String, tf_idf_unfolded: (List[(String, Double)], Map[String, Double])): List[(String, (String,Double))] = {
    for (term_freq <- tf_idf_unfolded._1) 
      yield ( (file_name, (term_freq._1, term_freq._2 * tf_idf_unfolded._2(term_freq._1))))
  }
  
  /*	Given two maps representing the frequency counts of the words of two files, it computes its cosine similarity
   * 	@param m1 First document
   * 	@param m2 Second document
   */
  def cosinesim2(m1: Map[String, Double], m2: Map[String, Double]): Double = {
    
    //aligning vectors
    val smv1 = m2.map({ case (key, value) => (key, 0.0)}) ++ m1
    val smv2 = m1.map({ case (key, value) => (key, 0.0)}) ++ m2
    
    //simplify vectors
    val smv1_vec = smv1.values.map(x => x.asInstanceOf[Double])
    val smv2_vec = smv2.values.map(x => x.asInstanceOf[Double])
    
    //compute cosinesim
    val term = smv1_vec.zip(smv2_vec).map(x => x._1 * x._2).reduceLeft( _ + _ )
    
    term / (FirstHalf.euclidean_norm(smv1_vec) * FirstHalf.euclidean_norm(smv2_vec))
  }
  
  /*	Given a pair of file names, and its representing space model vectors, computes its cosine similarity
   * 	@param files Pair of file names
   * 	@param tf_idfs Pair of space model vectors
   */
  def mapping4(files: (String, String), tf_idfs:(List[(String, Double)], List[(String, Double)]) ) = {
    val cosinesim = cosinesim2(tf_idfs._1.toMap, tf_idfs._2.toMap)
    
    List((files, cosinesim))
  }
  
  /*	Doesn't reduce. It's simply a pass through for the @p second parameter
   * 	@param first First parameter
   * 	@param second Second parameter
   */
  def passThroughtReduce[A](first: Any, second: List[A]) = {
    second
  }
  
  def main1() = {
    
    println("Calculs iniciats...")
    val tstart = System.nanoTime
    
    val nFiles = 100 //For illustration purposes.
    
    val stopwords = FirstHalf.readFile("stopwordscat-utf8.txt").split(" +").toList
    val files = Main.openFiles("wiki-xml-2ww5k", "", ".xml").take(nFiles)
    implicit val timeout = Timeout(10 days)
    
    val nMappers = 10
    val nReducers = 10
    
    val input = ( for( file <- files) yield (file.getName, (file.getAbsolutePath, stopwords)) ).toList

    val system = ActorSystem("TextAnalizer2")

    var master = system.actorOf(Props(new MapReduceActor[String, (String, List[String]), String, (String, Double)](input, mapping1, reducing1, nMappers, nReducers)))
    
    val futureResponse1 = master ? "start"
    val tf = Await.result(futureResponse1, timeout.duration).asInstanceOf[Map[String, List[(String, Double)]]]
    
    println("TFs calculats!")
    
    system.stop(master)
    
    master = system.actorOf(Props(new MapReduceActor[String, List[(String, Double)], String, String](tf.toList, mapping2, passThroughtReduce[String], nMappers, nReducers)))
    
    val futureResponse2 = master ? "start"
    val df = Await.result(futureResponse2, timeout.duration).asInstanceOf[Map[String, List[String]]]
    
    println("DFs calculats. Ara falten fer els IDFs")
    
    system.stop(master)
    
    //this line could be done with MapReduce, but the overhead caused by map and reduce actor initialization is not worth the time
    val idf = df.map(x => (x._1, Math.log(tf.size/x._2.length)))
    
    
    //input: List[File -> ( List[(Word, dtf)], List[(Word, idf)])]
    val tfIdfInput = tf.map({case (k,v) => (k, (v,idf))})
    
    master = system.actorOf(Props(new MapReduceActor[String, (List[(String, Double)], Map[String, Double]), String, (String, Double)](tfIdfInput.toList, mapping3, passThroughtReduce[(String,Double)], nMappers, nReducers)))
    
    val futureResponse3 = master ? "start"
    val tf_idf = Await.result(futureResponse3, timeout.duration).asInstanceOf[Map[String, List[(String, Double)]]]
    
    println("TF_IDFs calculats! Comparem els fitxers!")
    
    system.stop(master)
    
    //Map[(FileName, FileName) -> (List[(Word, tfidf)], List[(Word, tfidf)])
    val comparisonList = for ( pair <- tf_idf.toSet.subsets(2) ) yield {
      ((pair.head._1, pair.drop(1).head._1), (pair.head._2, pair.drop(1).head._2))
    }
    
    master = system.actorOf(Props(new MapReduceActor[(String, String), (List[(String, Double)], List[(String, Double)]), (String, String), Double](comparisonList.toList, mapping4, passThroughtReduce[Double], nMappers, nReducers)))
    
    val futureResponse4 = master ? "start"
    val result = Await.result(futureResponse4, timeout.duration).asInstanceOf[Map[(String, String), List[Double]]]
    
    val finalResult = result.map({case (k,v) => (k, v.apply(0))})
    
    val tend = System.nanoTime
    
    println("Resultats del calcul de similaritat entre documents: ")
    
    for(singleResult <- finalResult){
      println("Els documents " + singleResult._1._1 + " i " + singleResult._1._2 + " tenen una similaritat del " + singleResult._2*100 + "%")
    }
    
    println("Calculs finalitzats. Temps total: " + (tend-tstart)/Math.pow(10,9))
    
    system.shutdown
  }
}