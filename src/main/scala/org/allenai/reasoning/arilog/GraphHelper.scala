package org.allenai.reasoning.arilog

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.{Graph, Direction, Edge, Vertex}
import org.allenai.reasoning.arilog.RuleParts.RulePart
import scala.collection.JavaConversions._

object GraphHelper {
  
  def prettyString(graph:TinkerGraph) = {
    import scala.collection.JavaConversions._
    def vertexString(vertex: Vertex) = s"${vertex.getId()}:[${vertex.getProperty("isa")} (${vertex.getProperty("RulePart")})]"
    graph.getEdges.map(edge => {
      s"${vertexString(edge.getVertex(Direction.OUT))} -${edge.getLabel}-> ${vertexString(edge.getVertex(Direction.IN))}"
    }).mkString("\n")
  }

  def antecedentEdges(g:Graph) = g.getEdges.filter(e => isRulePartEdge(e, RuleParts.Antecedent))
  def consequentEdges(g:Graph) = g.getEdges.filter(e => isRulePartEdge(e, RuleParts.Consequent))

  def isRulePartEdge(e:Edge, rulePart:RulePart) = {
    isRulePartVertex(e.getVertex(Direction.IN), rulePart) || isRulePartVertex(e.getVertex(Direction.OUT), rulePart)
  }
  def isRulePartVertex(v:Vertex, rulePart:RulePart) = {
    rulePart.toString.equals(v.getProperty[String]("RulePart"))
  }

  /** Create graph from Rule. */
  def createGraph(rule:Rule): TinkerGraph = {
    val graph = new TinkerGraph
    val isaPredicates = (rule.antecedents ++ rule.consequents).filter(_.predicate.equals("isa"))
    val isaMap = isaPredicates.map(t => t.arg1 -> t.arg2).toMap
    //Hack to handle one-level of isa transitivity.
    val newIsaMap = isaMap.map(kv => {
      kv._1 -> isaMap.getOrElse(kv._2, kv._2)
    }).toMap

    def addToGraph(predicates:Seq[Literal], rulePart:RulePart, graph:TinkerGraph) = {
      var entities = Map[String, Entity]()
      val nonIsaPredicates = predicates.filter(!_.predicate.equals("isa"))
      def entity(id:String) = entities.get(id) match {
        case Some(entity:Entity) => entity
        case None => {
          val entity = Entity(id, newIsaMap.get(id), Map("RulePart" -> rulePart.toString))
          entities += (id -> entity)
          entity
        }
      }
      nonIsaPredicates.foreach(triple => {
        val arg1Vertex = addVertex(entity(triple.arg1), graph)
        val arg2Vertex = addVertex(entity(triple.arg2), graph)
        addEdge(arg1Vertex, arg2Vertex, triple.predicate, graph)
      })
    }
    addToGraph(rule.antecedents, RuleParts.Antecedent, graph)
    addToGraph(rule.consequents, RuleParts.Consequent, graph)
    graph
  }

  //Edges are indexed by a combination of the
  //source, target ids and the label on the edge.
  def edgeKey(s:String, t:String, l:String) = s"${s};${t};${l}"

  //Add edge if it doesn't exist already.
  def addEdge(src:Vertex, tgt:Vertex, label:String, graph:TinkerGraph) = {
    val key = edgeKey(src.getId.toString, tgt.getId.toString, label)
    Option(graph.getEdge(key)) match {
      case Some(edge:Edge) => edge
      case None => graph.addEdge(key, src, tgt, label)
    }
  }
  //Add vertex if it doesn't exist already.
  def addVertex(entity: Entity, graph:TinkerGraph): Vertex = {
    Option(graph.getVertex(entity.id)) match {
      case Some(vertex:Vertex) => vertex
      case None => {
        val vertex = graph.addVertex(entity.id)
        vertex.setProperty("isa", entity.isa.getOrElse(""))
        entity.metaData.foreach(kv => vertex.setProperty(kv._1, kv._2))
        vertex
      }
    }
  }

}