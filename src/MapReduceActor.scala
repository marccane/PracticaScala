import akka.actor._

class MapReduceActor[K, V, K2, V2]
    (
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2],
    numMappers: Int,
    numReducers: Int                        ) extends Actor{
  
  case class MapOrder(k: K, v: V, mapping: (K, V) => List[(K2, V2)])
  case class ReduceOrder(k: K2, vlist: List[V2], reducing: (K2, List[V2]) => List[V2])
  
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
    case "start" =>
      invoker = sender
      
      if (input.length > 0) {
      
        val groupSize = if (input.length/numMappers == 0) 1 else input.length/numMappers
        val groups = input.grouped(groupSize)

        var workers: List[akka.actor.ActorRef] = List()
        
        for (group <- groups) {
          val worker = context.actorOf(Props(new Actor {
            def receive = {
              case MapOrder(key, value, mapping) => sender ! Intermediate(mapping(key, value))
            }
          }))
          
          workers ::= worker
          
          for ((key, value) <- group) worker ! MapOrder(key, value, mapping)
        }
        
        pendingIntermediates = input.length
      }
      else invoker ! Map()
      
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
        
        for (group <- groups){
          val worker = context.actorOf(Props(new Actor {
            def receive = {
              case ReduceOrder(key, values, reducing) => sender ! Reduced(key, reducing(key, values))
            }
          }))
          
          workers ::= worker
          
          for ((key, values) <- group) worker ! ReduceOrder(key, values, reducing)
          
        } 
        
        pendingReduceds = dict.size
      }
      
    case Reduced(key, values) =>
      result += (key -> values)
      pendingReduceds -= 1
      
      if(pendingReduceds == 0)
        
        invoker ! result
  }
  
}