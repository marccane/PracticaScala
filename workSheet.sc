object workSheet {
  println("henry")
  readFile("file.txt")
  freq(readFile("file.txt"))
  
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val lines = try source.mkString finally source.close()
		
		"soc un valor de retorn stub"
  }
  
  def freq (s : String) : List[(String,Int)] = {
  	var stringList = s.split(' ');
  	val mapStrings: collection.mutable.Map[String, Int] = collection.mutable.Map();
  	
  	for ( i <- stringList ){
  	
  		var value: Option[Int] = mapStrings get i;
  		value match {
  			case Some(n) => mapStrings += ((i, n+1))
  			case None => mapStrings += (i -> 1)
  		}
  		
  	}
  	
  	List()
  }
  
}