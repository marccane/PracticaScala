
import scala.xml.XML
import scala.util.matching.Regex


object tractaxmldoc{
  
	def exempleMateu() {
	  val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream("wiki-xml-2ww5k/32509.xml"), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  // obtinc el titol
	  val titol=(xmllegg \\ "title").text
	  // obtinc el contingut de la p�gina
	  val contingut = (xmllegg \\ "text").text
	  
	  // identifico refer�ncies
	  val ref = new Regex("\\[\\[[^\\]]*\\]\\]") 
	  println("La pagina es: " + titol)
	  //println("i el contingut: ")
	  //println(contingut)
	  val refs=(ref findAllIn contingut).toList
	  
	  // elimino les que tenen :
	  val kk = refs.filterNot(x=> x.contains(':')) //A kk queden totes les strings que compleixen el regex i no contenen ':'
	  
	  val links = refs.filter(x=> x.contains(':'))
	  // caldr� eliminar-ne m�s?
	  
	  for (r <- kk) println(r)
	  println(refs.length)
	  println(kk.length)
	}
	
	def titolsIRefs(){
	  val files = Main.openFiles("smallxml", "", ".xml")
    //val input = for(file <- files) yield (file.getName, FirstHalf.readFile(file.getAbsolutePath).split(" +").toList)
	  
	  for(file<-files){
  	  val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(file), "UTF-8")
  	  val xmllegg = XML.load(xmlleg)
  	  val titol=(xmllegg \\ "title").text
  	  print("Titol: " + titol + " Referencies: ")
  	  
  	  val contingut = (xmllegg \\ "text").text 
  	  val ref = new Regex("\\[\\[[^\\]]*\\]\\]") 
  	  val refs=(ref findAllIn contingut).toList
  	  
  	  //cuidao: l'ordre d'aquestes operacions IMPORTA
  	  val kk3 = refs.filterNot(x=> x.contains(':') || x.apply(2)=='#').map(x=>x.substring(2,x.length()-2)).map(x=>x.split("\\|").apply(0)).map(x=>x.split("#").apply(0))
  	  //CUIDAO: hi haurà referències a pàgines que no tenim!
  	  
  	  for (r <- kk3) print("'" + r + "' ")
  	  println()
	  }
	}
	
  def referencies(file: java.io.File): (String, List[String]) = {
    val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(file), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  val titol=(xmllegg \\ "title").text
	  
	  val contingut = (xmllegg \\ "text").text 
	  val ref = new Regex("\\[\\[[^\\]]*\\]\\]") 
	  val refs=(ref findAllIn contingut).toList
	  
	  //cuidao: l'ordre d'aquestes operacions IMPORTA
	  val kk3 = refs.filterNot(x=> x.contains(':') || x.apply(2)=='#').map(x=>x.substring(2,x.length()-2)).map(x=>x.split("\\|").apply(0)).map(x=>x.split("#").apply(0))
	  //CUIDAO: hi haurà referències a pàgines que no tenim!
	  
	  (titol, kk3)
  }
	 
  def titols(files: List[java.io.File]): List[String] = 
    for(file <- files) yield {
      val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(file), "UTF-8")
  	  val xmllegg = XML.load(xmlleg)
  	  val titol=(xmllegg \\ "title").text
  	  titol
    }
	  
  def acceptableChar(c: Char): Boolean = c.isLetter || c == ' ' //|| c == '\''
  
  //Given an xml file, extracts the contents of the text region and produces a representative string that only contains lower case characters and spaces
  def readXMLFile(filename : String) : String = {
	  val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream(filename), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  //val titol=(xmllegg \\ "title").text
	  val contingut = (xmllegg \\ "text").text
	  
		val str = try contingut.map(c => if(acceptableChar(c)) c else ' ').mkString finally xmlleg.close()
		str.toLowerCase.trim.replaceAll(" +", " ")
  }
  
}