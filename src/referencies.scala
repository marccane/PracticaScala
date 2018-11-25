
import scala.xml.XML
import scala.util.matching.Regex


object tractaxmldoc extends App {
	def main() {
	  val xmlleg=new java.io.InputStreamReader(new java.io.FileInputStream("wiki-xml-2ww5k/32509.xml"), "UTF-8")
	  val xmllegg = XML.load(xmlleg)
	  // obtinc el titol
	  val titol=(xmllegg \\ "title").text
	  // obtinc el contingut de la p�gina
	  val contingut = (xmllegg \\ "text").text
	  
	  // identifico refer�ncies
	  val ref = new Regex("\\[\\[[^\\]]*\\]\\]") 
	  println("La pagina es: " + titol)
	  println("i el contingut: ")
	  println(contingut)
	  val refs=(ref findAllIn contingut).toList
	  
	  // elimino les que tenen :
	  val kk = refs.filterNot(x=> x.contains(':'))
	  
	  // caldr� eliminar-ne m�s?
	  
	  for (r<-refs) println(r)
	  println(refs.length)
	  println(kk.length)
	}
}