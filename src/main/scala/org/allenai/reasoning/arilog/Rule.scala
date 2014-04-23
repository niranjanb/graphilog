package org.allenai.reasoning.arilog

import scala.collection.JavaConversions._
import edu.washington.cs.knowitall.morpha.MorphaStemmer

case class Literal(predicate:String, arg1:String, arg2:String){
  def prettyString:String = s"$predicate($arg1, $arg2)"
}
case class Rule(id:String, antecedents:Seq[Literal], consequents:Seq[Literal]){
  def prettyString:String = s"Id:${id}: Antecedents: ${antecedents.map(_.prettyString).mkString(",")}\tConsequents: ${consequents.map(_.prettyString).mkString(",")}"
}

object RuleParts extends Enumeration {
  type RulePart = Value
  val Antecedent, Consequent = Value
}

object Rule{

  val ruleIdRe = """^(.*?)::(.*)""".r
  val ruleRe = """(.*?)->(.*)""".r
  val predicatesRe = """[\s,]*(.*?)\((.*?), (.*?)\)""".r

  def literals(clause:String): Seq[Literal] =  {
    import MorphaStemmer._
    def scrubAndStem(string:String) = stem(string.replaceAll("""\"""", ""))
    val out = for{ pred <- predicatesRe.findAllMatchIn(clause)
      predicate = scrubAndStem(pred.group(1))
      arg1 = scrubAndStem(pred.group(2))
      arg2 = scrubAndStem(pred.group(3))
    }yield{
      Literal(predicate, arg1, arg2)
    }
    out.toSeq
  }
  def apply(line:String): Option[Rule] = {
    //rule11:: isa(E17, "eat"), isa(E4, "humans"), isa(E19, "healthy foods"), agent(E17, E4), object(E17, E19)
    // -> effect(E17, E12), isa(E12, "be"), isa(E13, "healthy"), agent(E12, E4), object(E12, E13).
    try{
      val (id, content) = line match {
        case ruleIdRe(ruleId:String, rest:String)  => (ruleId, rest)
        case _ => ("", line)
      }
      val rule = content.toLowerCase match {
        case ruleRe(antecedents:String, consequents:String) => Rule(id, literals(antecedents), literals(consequents))
        case _ => Rule(id, Seq(), literals(content))
      }
      Some(rule)
    } catch {
      case e:Exception => None
    }
  }
}