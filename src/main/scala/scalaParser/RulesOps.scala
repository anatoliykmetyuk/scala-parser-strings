package scalaParser

trait RulesOps {

  type S = String

  def Concat : (S, S)                => S = _+_
  def Concat3: (S, S, S)             => S = _+_+_
  def Concat4: (S, S, S, S)          => S = _+_+_+_
  def Concat5: (S, S, S, S, S)       => S = _+_+_+_+_
  def Concat7: (S, S, S, S, S, S, S) => S = _+_+_+_+_+_+_

  def ConcatSeq(delimiter: String): (Seq[String]) => String = _.mkString(delimiter)

  def ConcatSeqSemi    = ConcatSeq(";")
  def ConcatSeqNoDelim = ConcatSeq("" )
  def ConcatSeqComma   = ConcatSeq(",")
  def ConcatSeqDot     = ConcatSeq(".")
  def ConcatSeqWith    = ConcatSeq(" with ")

  def ExtractOpt: Option[String] => String = _.getOrElse("")

}