object Main extends App {
  case class WordLog(word: String, count: Int){
    def frequency(total: Int) = (count toFloat) / total
  }
  
  //Given a file, produces a representative string that only contains lower case characters and spaces
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val str = try source filter (c => c.isLetter || c == ' ') map (c => c.toLower) mkString finally source.close()
		str
  }
  
  def freq (s : String) : List[(String,Int)] = {
    val wordMap: collection.mutable.Map[String, Int] = collection.mutable.Map() withDefaultValue(0);
    
    for ( i <- s.split(' ') ) wordMap(i) += 1 //counting words
    
    ((for (k <- wordMap.keys) yield (k,wordMap(k))) toList) sortWith(_._2 > _._2)
    //convert the map to a (descending ordered by tuples' second element) list
  }  
  
  override def main(args:Array[String]) =  { 
    
    println("Enter a file name: ")
    //val fileName = scala.io.StdIn.readLine()
    //val freqCounts = freq(readFile(fileName))
    val freqCounts = freq(readFile("file.txt"))
    val nWords = freqCounts.foldLeft(0) { (total, actual) => total + actual._2 } 
    val nDiffWords = freqCounts length;
    println("N Words: " + nWords + " Diferent: " + nDiffWords)
    println("Words		" + " ocurrences " + " frequency")
    for(r <- freqCounts) println(r._1 + "			" + r._2 + "	" + r._2.toFloat/nWords)
  }
}