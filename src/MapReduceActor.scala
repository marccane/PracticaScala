import akka.actor._

/*	Class extending akka Actor containing an implementation of MapReduce design pattern. 
 * 	To start the algorithm you have to instantiate an actor of this class, and then send him the message "start".
 * 	Eventually it will compute and return to you the result.
 * 	@param input List of pairs key value where the mapping function will be applied
 * 	@param mapping Mapping function
 * 	@param reducing Reduction function
 * 	@param numMappers Number of actors that will be assigned to mapping operations
 *  @param numReducers Number of actors that will be assigned to reducing operations
 */
class MapReduceActor[K, V, K2, V2]
    (
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2],
    numMappers: Int,
    numReducers: Int                        ) extends Actor{
  
  //master - service messages
  case class MapOrder(k: K, v: V, mapping: (K, V) => List[(K2, V2)])
  case class ReduceOrder(k: K2, vlist: List[V2], reducing: (K2, List[V2]) => List[V2])
  
  //service - master messages
  case class Intermediate(list: List[(K2, V2)])
  case class Reduced(key: K2, list: List[V2]) 
  
  val master = self
  var invoker: akka.actor.ActorRef = null
  
  var intermediates = List[(K2, V2)]()
  var pendingIntermediates: Int = 0
  
  var result = Map[K2, List[V2]]()
  var pendingReduceds: Int = 0
  
  //unused
  def cleanActors() = {
    for (child <- context.children){
      context.stop(child)
    }
  }
  
  def receive = {
    //first call
    case "start" =>
      invoker = sender
      
      if (input.length > 0) { //if there's something to compute
      
        val groupSize = if (input.length/numMappers == 0) 1 else input.length/numMappers
        val groups = input.grouped(groupSize)

        var workers: List[akka.actor.ActorRef] = List()
        
        //split the load between the actors
        for (group <- groups) {
          val worker = context.actorOf(Props(new Actor {
            def receive = {
              case MapOrder(key, value, mapping) => sender ! Intermediate(mapping(key, value))
            }
          }))
          
          workers ::= worker
          
          //order actors to map the pairs key value
          for ((key, value) <- group) worker ! MapOrder(key, value, mapping)
        }
        
        pendingIntermediates = input.length
      }
      else invoker ! Map()
     
    //Between mapping and reducing
    case Intermediate(list) => 
      intermediates = list ::: intermediates
      pendingIntermediates -= 1
      
      if(pendingIntermediates == 0){
        
        var dict = Map[K2, List[V2]]() withDefault (k => List())
      
        for ((key, value) <- intermediates)
          dict += (key -> (value :: dict(key)))
        
        var workers: List[akka.actor.ActorRef] = List() 
        
        val groupSize = if (dict.size/numMappers == 0) 1 else dict.size/numMappers
        val groups = dict.grouped(groupSize)
        
        //split the load between the actors
        for (group <- groups){
          val worker = context.actorOf(Props(new Actor {
            def receive = {
              case ReduceOrder(key, values, reducing) => sender ! Reduced(key, reducing(key, values))
            }
          }))
          
          workers ::= worker
          
          //order actors to reduce the values
          for ((key, values) <- group) worker ! ReduceOrder(key, values, reducing)
          
        } 
        
        pendingReduceds = dict.size
      }
    
    //adding up the results
    case Reduced(key, values) =>
      result += (key -> values)
      pendingReduceds -= 1
      
      if(pendingReduceds == 0) //if we have finished, send back to invoker the results
        
        invoker ! result
  }
  
}