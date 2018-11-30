import scala.xml.XML
import scala.util.matching.Regex

object tractaxmldoc{
  
  /*	Given a file handler of an xml file returns a tuple with the title and the list of references found
   * 	@param file The file handler of the xml file
   */
  def referencies(file: java.io.File): (String, List[String]) = {
    val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(file), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  val titol=(xmllegg \\ "title").text
	  
	  val contingut = (xmllegg \\ "text").text 
	  val ref = new Regex("\\[\\[[^\\]]*\\]\\]") 
	  val refs=(ref findAllIn contingut).toList
	  
	  //l'ordre d'aquestes operacions IMPORTA, hi haurà referències a pàgines que no tenim!
	  val kk3 = refs.filterNot(x=> x.contains(':') || x.apply(2)=='#').map(x=>x.substring(2,x.length()-2)).map(x=>x.split("\\|").apply(0)).map(x=>x.split("#").apply(0))
	  
	  (titol, kk3)
  }
	 
  /*	Given an array of xml file handlers return the titles of the files
   * 	@param files The array of xml file handlers
   */
  def titols(files: List[java.io.File]): List[String] = 
    for(file <- files) yield {
      val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(file), "UTF-8")
  	  val xmllegg = XML.load(xmlleg)
  	  val titol=(xmllegg \\ "title").text
  	  titol
    }
  
  /*	Given an xml file, extracts the contents of the text region and produces a representative string that only contains lower case characters and spaces
   * 	@param filename The name of the xml file to be read
   */
  def readXMLFile(filename : String) : String = {
	  val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(filename), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  //val titol=(xmllegg \\ "title").text
	  val contingut = (xmllegg \\ "text").text
	  
		val str = try contingut.map(c => if(FirstHalf.acceptableChar(c)) c else ' ').mkString finally xmlleg.close()
		str.toLowerCase.trim.replaceAll(" +", " ")
  }
  
}