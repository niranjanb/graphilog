package org.allenai.reasoning.arilog

import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import java.io.File
import scala.io.Source


trait Term{
  val id:String
  val isa:Option[String]
  val metaData:Map[String, String]
}
case class Entity(id:String, isa:Option[String], metaData:Map[String, String]) extends Term
case class Event(id:String, isa:Option[String], metaData:Map[String, String]) extends Term

case class KBEntry(id:String, english:String, rule:Rule, graph:TinkerGraph) {
  def prettyString: String = {
    s"Id:${id}\nEnglish:${english}\n${rule.prettyString}\n${LFGraphHelper.prettyString(graph)}"
  }
}

object LFParser {
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
          Some(KBEntry(rule.id, english, rule, LFGraphHelper.createGraph(rule)))
        }
        case _ => None
      }
    })
  }
}

object LFParserTest{
  val string1 = """rule1:: isa(E17, "eat"), isa(E4, "humans"), isa(E19, "healthy foods"), for(E17, E4), object(E17, E19) -> effect(E17, E12), isa(E12, "be"), isa(E13, "healthy"), agent(E12, E4), object(E12, E13)."""
  testGraph(string1)
  val string2 = """rule1:: isa(E17, "eat"), isa(E4, "humans"), isa(E19, "healthy foods"), for(E17, E4), object(E17, E19)."""
  testGraph(string2)

  def testGraph(string:String) {
    import LFGraphHelper._
    Rule(string) match {
      case Some(rule:Rule) => println(prettyString(createGraph(rule)))
      case None => println(s"Failed to extract rule from: ${string}")
    }
  }
}
