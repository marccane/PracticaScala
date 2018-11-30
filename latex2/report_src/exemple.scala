 object MapReduceEnric{
  def mapping1(file_name: String, file: (String, List[String])): List[(String, (String, Int))] = {
    val wordList = FirstHalf.readFile(file._1).split(" +").toList.filterNot(file._2.contains(_))
    
    val x = (for(word <- wordList) yield (file_name, (word, 1)))//.groupBy(_._1)
    x
  }
  
  //key-> Filename, values-> list of (Word, count)
  def reducing1(key: String, values: List[(String, Int)]): List[(String, Int)] = {
    val res = for( (word, count_list) <- values.groupBy(_._1).toList ) 
      //For every pair of word and list of counts, add up its counts
      yield (word, count_list.map( {case (_, count) => count } ).reduceLeft( _ + _))
      
    res.sortWith(FirstHalf.moreFrequent)
  }
  
  def main1() = {
    val stopwords = FirstHalf.readFile("test/english-stop.txt").split(" +").toList
    val files = Main.openPgTxtFiles("test", "pg", ".txt")
    
    val input = ( for( file <- files) yield (file.getName, (file.getAbsolutePath, stopwords)) ).toList

    val system = ActorSystem("TextAnalizer2")

    val master = system.actorOf(Props(new MapReduceActor[String, (String, List[String]), String, (String, Int)](input, MapReduceEnric.mapping1, MapReduceEnric.reducing1, 2, 2)))
    implicit val timeout = Timeout(10 days)
    val futureResponse = master ? "start"
    val result = Await.result(futureResponse, timeout.duration)
    system.shutdown
    print(result)
  
  }