import akka.actor
/*
class MapReduce {
	def
	mapReduceBasic[K, V, K2, V2](			input: List[(K, V)],			mapping: (K, V) =>
	List[(K2, V2)],
	reducing: (K2, List[V2])
	=>
	List[V2]
			): Map[K2, List[V2]] = {
					// Definim Intermediate a dintre per
					// fer-lo depenent de K2, V2
			case class
			Intermediate(list: List[(K2, V2)])
			val master = self
			val	workers = for	((key, value) <- input) yield actor {
				master ! Intermediate(mapping(key, value))
			}
	}*/

object MapReduce{
  /*def mapreduce[K, V, K2, V2]( input: List[(K, V)],
	    mapping: (K, V)	=> List[(K2, V2)],
	    reducing: (K2, List[V2]) => List[V2]
	    ): Map[K2, List[V2]] = {
  	case class Intermediate(list: List[(K2, V2)])
  	case class Reduced(key: K2, values: List[V2])
  	val master = this
  	this.trapExit = true
  	// var assignedMappers = Map[Actor, (K, V)]()
  	var assignedMappers = Map[AbstractActor, (K, V)]()
  	def spawnMapper(key: K, value: V) = {
  			val mapper = link {
  				master ! Intermediate(mapping(key, value))
  			}
  			assignedMappers += (mapper -> (key, value))
  		  mapper
  	}

  	for((key, value) <- input)
  		spawnMapper(key, value)
  		var intermediates = List[(K2, V2)]()
  		var nleft = input.length
  		while (nleft > 0) receive {
    		case Intermediate(list) => intermediates :::= list
    		case Exit(from, 'normal) => nleft -= 1
    		case Exit(from, reason) => // retrieve assigned work
    		val (key, value) = assignedMappers(from)
    		// spawn new worker to re-execute the work
    		spawnMapper(key, value)
  		}
	}*/
  

}