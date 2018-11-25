import akka.actor._

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
  
  def freq (s : String) : List[(String,Int)] = ngramsfreq(s, 1)
  
  def nonstopfreq (s: String, stopwords: List[String]): List[(String,Int)] = 
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
    
  def ngramsfreq(s: String, n: Int): List[(String,Int)] = {
    val wordMap: collection.mutable.Map[String, Int] = collection.mutable.Map() withDefaultValue(0);
    
    for ( i <- s.split(" +").sliding(n,1) ) wordMap(i.mkString(" ")) += 1 //counting words
    
    wordMap.toList
  }
  
  def topN(freqencyList: List[(String, Int)], n: Int) = {
    val nWords = freqencyList.foldLeft(0) { (total, actual) => total + actual._2 } 
    val nDiffWords = freqencyList length;
    println("N Words: " + nWords + " Diferent: " + nDiffWords)
    println("Words		" + " ocurrences " + " frequency")
    for(r <- freqencyList.slice(0,n)) println(r._1 + "			" + r._2 + "	" + (r._2.toFloat/nWords)*100)
  }
  
  def cosinesim(s1: String, s2: String, stopwords: List[String]): Double = {
    val freq1 = nonstopfreq(s1, stopwords); val freq2 = nonstopfreq(s2, stopwords);
    val freq1max = freq1.maxBy(_._2)._2; val freq2max = freq2.maxBy(_._2)._2;
    var freq1norm = freq1.map(x => (x._1, x._2.toFloat/freq1max))
    var freq2norm = freq2.map(x => (x._1, x._2.toFloat/freq2max))
    
    
    //align vectors 1/2 O(2n^2)
    //falta arreglar les tuples que es fiquen
    for(w <- freq1norm) if (freq2norm.filter(_._1 == w._1).length == 0) freq2norm = (w._1, 0.toFloat)::freq2norm
    for(w <- freq2norm) if (freq1norm.filter(_._1 == w._1).length == 0) freq1norm = (w._1, 0.toFloat)::freq1norm
    //align vectors 2/2 O(2nLogn)
    val smv1=freq1norm.sortWith(_._1 < _._1).map(_._2)
    val smv2=freq2norm.sortWith(_._1 < _._1).map(_._2)
    
    var sim = (smv1.zip(smv2).map({case (x,y) => x*y}).foldLeft(0.toFloat)(_ + _))
    val firstroot = Math.sqrt( smv1.map(x => Math.pow(x,2).toFloat).foldLeft(0.toFloat)(_ + _) )
    val secondroot = Math.sqrt( smv2.map(x => Math.pow(x,2).toFloat).foldLeft(0.toFloat)(_ + _) )
    sim /= (firstroot*secondroot).toFloat

    sim
  }
  /*
  def mapReduceBasic[K, V, K2, V2](
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2]): Map[K2, List[V2]] = {
    case class Intermediate(list: List[(K2, V2)])
    val master = self
    val workers = for ((key, value) <- input) yield actor {
      master ! Intermediate(mapping(key, value))
    }
    var intermediates = List[(K2, V2)]()
    for (_ <- 1 to input.length)
      receive {
        case Intermediate(list) => intermediates :::= list
      }
    var dict = Map[K2, List[V2]]() withDefault (k => List())
    for ((key, value) <- intermediates)
      dict += (key -> (value :: dict(key)))
    var result = Map[K2, List[V2]]()
    for ((key, value) <- dict)
      result += (key -> reducing(key, value))
    result
  }*/
  

  
  def compareAll(folderName: String): Any = {
    var fileList = new java.io.File(folderName).listFiles//.filter(_.getName.endsWith(".xml"))
    for( i <- 0 to fileList.length -1 ){
      for( j <- i+1 to fileList.length -1 ){
        println(fileList.apply(i).getName + " -> " + fileList.apply(j).getName )
      }
    }
    
    "No se que retornara"
  }
  /*
  override def main(args:Array[String]) =  {
    compareAll("test")
  }*/
  
  class HelloActor extends Actor {
  def receive = {
    case "hello" => println("hello back at you")
    case _       => println("huh?")
    }
  }
  
  override def main(args:Array[String]) =  {
    //compareAll("test")
    //tractaxmldoc.main
    
    /*println("Enter a file name: ")
    val fileName = scala.io.StdIn.readLine()
    val freqCounts = freq(readFile(fileName))*/
    /*val s = readFile("test/pg11.txt")
    val s2 = readFile("test/pg11-net.txt")*/
    /*val freqCounts = freq(s).sortWith(_._2 > _._2)
    topN(freqCounts, 10)
    val nonStopFreqCounts = nonstopfreq(s, readFile("test/english-stop.txt").split(" +").toList).sortWith(_._2 > _._2)
    topN(nonStopFreqCounts, 10)
    paraulafreqfreq(s, 10, 5)
    topN(ngramsfreq(s, 3), 10)
    println("Cosine sim: ")
    val ini =System.nanoTime()
    println( cosinesim(readFile("test/pg11.txt"),readFile("test/pg11-net.txt"), readFile("test/english-stop.txt").split(" +").toList) )
    val fi =System.nanoTime()
    println("Time: " + (fi-ini).toDouble/1000000000)
    */
      val system = ActorSystem("HelloSystem")
      // default Actor constructor
      
      val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
      helloActor ! "hello"
      helloActor ! "buenos dias"
  }
}