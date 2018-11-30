
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