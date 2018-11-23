object Main extends App {
  case class WordLog(word: String, count: Int){
    def frequency(total: Int) = (count toFloat) / total
  }
  
  def acceptableChar(c: Char): Boolean = c.isLetter || c == ' ' //|| c == '\''
  
  //Given a file, produces a representative string that only contains lower case characters and spaces
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val str = try source.map(c => if(acceptableChar(c)) c else ' ').mkString finally source.close()
		str.toLowerCase.trim
  }
  
  def freq (s : String) : List[(String,Int)] = {
    val wordMap: collection.mutable.Map[String, Int] = collection.mutable.Map() withDefaultValue(0);
    
    for ( i <- s.split(" +") ) wordMap(i) += 1 //counting words
    
    ((for (k <- wordMap.keys) yield (k,wordMap(k))) toList) sortWith(_._2 > _._2)
    //convert the map to a (descending ordered by tuples' second element) list
  }
  
  def nonstopfreq (s: String, stopwords: List[String]) = 
    freq(s).filterNot(x => stopwords.contains(x._1))
    
  def paraulafreqfreq(s: String, nMost: Int, nLeast: Int) = {
    val freqfreqMap: collection.mutable.Map[Int, Int] = collection.mutable.Map() withDefaultValue(0)
    val frequencies = freq(s)
    for(wordFreq <- frequencies) freqfreqMap(wordFreq._2) += 1
    
    val freqList = freqfreqMap.toList.sortWith((x,y) => x._2 > y._2 || (x._2 == y._2 && x._1 < y._1))
    println("Les " + nMost + " frequencies mes frequents:")
    for(elem <- freqList.slice(0, nMost)) println(elem._2 + " paraules apareixen " + elem._1)
    println("Les " + nLeast + " frequencies menys frequents:")
    for(elem <- freqList.slice(freqList.length-nLeast, freqList.length)) println(elem._2 + " paraules apareixen " + elem._1 + " vegades")
  }
    
  def topN(freqencyList: List[(String, Int)], n: Int) = {
    val nWords = freqencyList.foldLeft(0) { (total, actual) => total + actual._2 } 
    val nDiffWords = freqencyList length;
    println("N Words: " + nWords + " Diferent: " + nDiffWords)
    println("Words		" + " ocurrences " + " frequency")
    for(r <- freqencyList.slice(0,n)) println(r._1 + "			" + r._2 + "	" + (r._2.toFloat/nWords)*100)
  } 
  
  override def main(args:Array[String]) =  { 
    /*println("Enter a file name: ")
    val fileName = scala.io.StdIn.readLine()
    val freqCounts = freq(readFile(fileName))*/
    val s = readFile("test/pg11.txt")
    val freqCounts = freq(s)
    topN(freqCounts, 10)
    val nonStopFreqCounts = nonstopfreq(s, readFile("test/english-stop.txt").split(" +").toList)
    topN(nonStopFreqCounts, 10)
    paraulafreqfreq(s, 10, 5)
  }
}