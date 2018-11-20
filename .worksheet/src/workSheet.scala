object workSheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(37); 
  println("henry");$skip(23); val res$0 = 
  readFile("file.txt");System.out.println("""res0: String = """ + $show(res$0));$skip(29); val res$1 = 
  freq(readFile("file.txt"));System.out.println("""res1: List[(String, Int)] = """ + $show(res$1));$skip(197); 
  
  def readFile(filename : String) : String = {
  	val source = scala.io.Source.fromFile(filename)
		val lines = try source.mkString finally source.close()
		
		"soc un valor de retorn stub"
  };System.out.println("""readFile: (filename: String)String""");$skip(391); 
  
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
  };System.out.println("""freq: (s: String)List[(String, Int)]""")}
  
}
