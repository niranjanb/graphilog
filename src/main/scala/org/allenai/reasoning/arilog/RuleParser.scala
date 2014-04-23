package org.allenai.reasoning.arilog

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import java.io.File
import scala.io.Source


//Terms are skolem constants
trait Term{
  val id:String
  val isa:Option[String]
  val metaData:Map[String, String]
}

//Entity skolems
case class Entity(id:String, isa:Option[String], metaData:Map[String, String]) extends Term

//Event skolems
case class Event(id:String, isa:Option[String], metaData:Map[String, String]) extends Term


//A KB entry consists of a rule and its graphical representation.
case class KBEntry(id:String, english:String, rule:Rule, graph:TinkerGraph) {
  def prettyString: String = {
    s"Id:${id}\nEnglish:${english}\n${rule.prettyString}\n${GraphHelper.prettyString(graph)}"
  }
}

//A parser for KB entries. 
object RuleParser {
  def parse(file:File): Seq[KBEntry] = {
    var englishMap = Map[String, String]()
    val commentRe = """^%.*""".r
    val englishRe = """^\s*english\((.*?),(.*?)\)\.""".r
    var otherLines = Seq[String]()
    val lines = Source.fromFile(file).getLines().filterNot(commentRe.pattern.matcher(_).matches).toSeq
    lines.foreach {
      case englishRe(id: String, english: String) => englishMap += id -> english
      case line => (otherLines :+= line)
    }
    otherLines.flatMap(line => {
      Rule(line) match {
        case Some(rule:Rule) => {
          val english = englishMap.getOrElse(rule.id, "")
          Some(KBEntry(rule.id, english, rule, GraphHelper.createGraph(rule)))
        }
        case _ => None
      }
    })
  }
}

object RuleParserTest extends App{
  val string1 = """rule1:: isa(E17, "eat"), isa(E4, "humans"), isa(E19, "healthy foods"), for(E17, E4), object(E17, E19) -> effect(E17, E12), isa(E12, "be"), isa(E13, "healthy"), agent(E12, E4), object(E12, E13)."""
  testGraph(string1)
  val string2 = """rule1:: isa(E17, "eat"), isa(E4, "humans"), isa(E19, "healthy foods"), for(E17, E4), object(E17, E19)."""
  testGraph(string2)

  def testGraph(string:String) {
    import GraphHelper._
    Rule(string) match {
      case Some(rule:Rule) => println(prettyString(createGraph(rule)))
      case None => println(s"Failed to extract rule from: ${string}")
    }
  }
}
