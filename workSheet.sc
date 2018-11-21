object workSheet {
  println("henry")                                //> henry
  println(readFile("file.txt"))                   //> java.io.FileNotFoundException: file.txt (El sistema no puede encontrar el arc
                                                  //| hivo especificado)
                                                  //| 	at java.io.FileInputStream.open0(Native Method)
                                                  //| 	at java.io.FileInputStream.open(Unknown Source)
                                                  //| 	at java.io.FileInputStream.<init>(Unknown Source)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:91)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:76)
                                                  //| 	at scala.io.Source$.fromFile(Source.scala:54)
                                                  //| 	at workSheet$.readFile$1(workSheet.scala:7)
                                                  //| 	at workSheet$.$anonfun$main$1(workSheet.scala:3)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$anonfun$$ex
                                                  //| ecute$1(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:76)
                                                  //| 	at workSheet$.main(workSheet.scala:1)
                                                  //| 	at workSheet.main(workSheet.scala)
  //freq(readFile("file.txt"))
  
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val lines = try source.mkString finally source.close()
		
		"soc un valor de retorn stub"
  }
  /*
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
  */
}