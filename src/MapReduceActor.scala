import akka.actor._

class MapReduceActor() extends Actor{
  def mapReduceBasic[K, V, K2, V2](
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2],
    numMappers: Int,
    numReducers: Int  ): Map[K2, List[V2]] = {
      
    case class Intermediate(list: List[(K2, V2)])
    case class Reduced(key: K2, list: List[V2])
    
    val master = self
    //self.trapExit = true
    val workers = for (group <- input.grouped(input.length/numMappers)) yield
      context.actorOf(Props(new Actor {
        for ((key, value) <- group)
          master ! Intermediate(mapping(key, value))
      }))

    
    /*val workers = for ((key, value) <- input) yield context.actorOf(Props(new Actor {
      master ! Intermediate(mapping(key, value))
    }))*/
    
    var intermediates = List[(K2, V2)]()
    
    for (_ <- 1 to input.length)
      Actor.receive = {
        case Intermediate(list) => intermediates = intermediates ::: list
      }
    
    var dict = Map[K2, List[V2]]() withDefault (k => List())
    
    for ((key, value) <- intermediates)
      dict += (key -> (value :: dict(key)))
      
    for (group <- dict.grouped(dict.size / numReducers))
      context.actorOf(Props(new Actor {
        for ((key, values) <- group)
          master ! Reduced(key, reducing(key, values))
      }))
  
    var result = Map[K2, List[V2]]()
    
    for (_ <- 1 to dict.size)
      Actor.receive {
        case Reduced(key, values) =>
          result += (key -> values)
      }
      
    result
    }
}

/*
class MapReduceFramework[K, V, K2, V2]{
case class FileProcessing(fileList: Array[java.io.File])

case class MapOrder[K, V, K2, V2](k: K, v: V, mapping: (K, V) => List[(K2, V2)])
case class Intermediate[K2,V2](list: List[(K2, V2)])
case class ReduceOrder[K,V](k: K, vlist: List[V], reducing: (K, List[V]) => List[V])
case class Reduced[V](list: List[V])

class MapWorker() extends Actor{
  def receive = {
    case MapOrder(k,v,mapping) => sender ! Intermediate(mapping(k,v))
  }
}

class ReduceWorker extends Actor{
  def receive = {
    case ReduceOrder(k,vlist,reducing) => sender ! Reduced(reducing(k,vlist))
  }
}

class MapReduceActor() extends Actor{
  
  var intermediates: List[(K2,V2)]= List();
  
  def receive = {
    case FileProcessing(fileList) => 
      //fer coses
    case Intermediate(list) => intermediates = list :: intermediates;
      
  }
  
  def mapReduceBasic[K, V, K2, V2](
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2],
    numMappers: Int,
    numReducers: Int  ): Map[K2, List[V2]] = {
    
    val master = self
    //self.trapExit = true
    val workers = for ((key, value) <- input) yield context.system.actorOf(Props[MapWorker]) ! (key,value)
    /*
    var intermediates = List[(K2, V2)]()
    for (_ <- 1 to input.length)
      receive {
        case Intermediate(list) => intermediates = intermediates ::: list
      }
    var dict = Map[K2, List[V2]]() withDefault (k => List())
    for ((key, value) <- intermediates)
      dict += (key -> (value :: dict(key)))
    var result = Map[K2, List[V2]]()
    for ((key, value) <- dict)
      result += (key -> reducing(key, value))
    result
    */
    Map()
    }
}
*/
/*
  def mapReduceBasic[K, V, K2, V2](
    input:    List[(K, V)],
    mapping:  (K, V) => List[(K2, V2)],
    reducing: (K2, List[V2]) => List[V2]): Map[K2, List[V2]] = {
    case class Intermediate(list: List[(K2, V2)])
    val master = self
    val workers = for ((key, value) <- input) yield actor {
      master ! Intermediate(mapping(key, value))
    }
    var intermediates = List[(K2, V2)]()
    for (_ <- 1 to input.length)
      receive {
        case Intermediate(list) => intermediates :::= list
      }
    var dict = Map[K2, List[V2]]() withDefault (k => List())
    for ((key, value) <- intermediates)
      dict += (key -> (value :: dict(key)))
    var result = Map[K2, List[V2]]()
    for ((key, value) <- dict)
      result += (key -> reducing(key, value))
    result
  }*/
}