package scalaParser
import acyclic.file
import language.implicitConversions
import syntax._
import org.parboiled2._

/**
 * Parser for Scala syntax.
 */
class Scala (val input: ParserInput)
  extends Core with Types with Exprs with Xml{

  private implicit def wspStr(s: String): R1 = rule( WL ~ capture(str(s)) ~> Concat )
  private implicit def wspCh (s: Char  ): R1 = rule( WL ~ capture(ch (s)) ~> Concat )

  def TmplBody: R1 = {
    def Prelude : R1 = rule( (Annot ~ OneNLMax ~> Concat).* ~> ConcatSeqNoDelim ~ (Mod.* ~> ConcatSeqNoDelim) ~> Concat )
    def TmplStat: R1 = rule( Import | Prelude ~ (BlockDef | Dcl) ~> Concat | StatCtx.Expr )
    def SelfType: R1 = rule( (`this` | Id | `_`) ~ ((`:` ~ InfixType ~> Concat).? ~> ExtractOpt) ~ `=>` ~> Concat3 )
    def TmplStatZero: R1 = rule ( (TmplStat ~ ((Semis ~ TmplStat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt )
    rule( '{' ~ (SelfType.? ~> ExtractOpt) ~ (Semis.? ~> ExtractOpt) ~ TmplStatZero ~ `}` ~> Concat5 )
  }

  def NewBody: R1 = rule( ClsTmpl | TmplBody )

  def ValRhs: R1 = {
    def Pat2One: R1 = rule ( Pat2 ~ ((',' ~ Pat2 ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( Pat2One ~ ((`:` ~ Type ~> Concat).? ~> ExtractOpt) ~ `=` ~ StatCtx.Expr ~> Concat4 )
  }
  def ValDef: R1 = rule( `val` ~ ValRhs ~> Concat )
  def VarDef: R1 = rule( `var` ~ Ids ~ `:` ~ Type ~ `=` ~ `_` ~> Concat6 | `var` ~ ValRhs ~> Concat )

  def DefDef: R1 = {
    def Body: R1 = rule( `=` ~ (`macro`.? ~> ExtractOpt) ~ StatCtx.Expr ~> Concat3 | OneNLMax ~ '{' ~ Block ~ "}" ~> Concat4 )
    rule( `def` ~ FunSig ~ ((`:` ~ Type ~> Concat).? ~> ExtractOpt) ~ Body ~> Concat4 )
  }

  def BlockDef: R1 = rule( DefDef | TypeDef | ValDef | VarDef | TraitDef | ClsDef | ObjDef )

  def ClsDef: R1 = {
    def ClsAnnot: R1 = rule( `@` ~ SimpleType ~ ArgList ~> Concat3 )
    def Prelude: R1 = rule( NotNewline ~ ( ClsAnnot.+ ~> ConcatSeqNoDelim ~ (AccessMod.? ~> ExtractOpt) ~> Concat | ClsAnnot.* ~> ConcatSeqNoDelim ~ AccessMod ~> Concat) ~> Concat )
    def ClsArgMod: R1 = rule( (Mod.* ~> ConcatSeqNoDelim ~ (`val` | `var`) ~> Concat).? ~> ExtractOpt )
    def ClsArg: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ ClsArgMod ~ Id ~ `:` ~ ParamType ~ ((`=` ~ ExprCtx.Expr ~> Concat).? ~> ExtractOpt) ~> Concat6 )

    // Comma-separated, ponentially without spaces!!!
    def Implicit: R1 = {
      def ClsArgOne: R1 = rule ( ClsArg ~ (("," ~ ClsArg ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
      rule( OneNLMax ~ '(' ~ `implicit` ~ ClsArgOne ~ ")" ~> Concat5 )
    }
    def ClsArgs: R1 = {
      def ClsArgZero: R1 = rule ( (ClsArg ~ ((',' ~ ClsArg ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt )
      rule( OneNLMax ~'(' ~ ClsArgZero ~ ")" ~> Concat4 )
    }
    def AllArgs: R1 = rule( ClsArgs.+ ~> ConcatSeqNoDelim ~ (Implicit.? ~> ExtractOpt) ~> Concat | Implicit )
    rule( `case`.? ~> ExtractOpt ~ `class` ~ Id ~ (TypeArgList.? ~> ExtractOpt) ~ (Prelude.? ~> ExtractOpt) ~ (AllArgs.? ~> ExtractOpt) ~ ClsTmplOpt ~> Concat7 )
  }
  def TraitDef: R1 = {
    def TraitTmplOpt: R1 = {
      def TraitParents: R1 = rule( AnnotType ~ ((`with` ~ AnnotType ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
      def TraitTmpl: R1 = rule( EarlyDefs.? ~> ExtractOpt ~ TraitParents ~ (TmplBody.? ~> ExtractOpt) ~> Concat3 )
      rule( `extends` ~ TraitTmpl ~> Concat | (`extends`.? ~> ExtractOpt ~ TmplBody ~> Concat).? ~> ExtractOpt )
    }
    rule( `trait` ~ Id ~ (TypeArgList.? ~> ExtractOpt) ~ TraitTmplOpt ~> Concat4 )
  }

  def ObjDef: R1 = rule( `case`.? ~> ExtractOpt ~ `object` ~ Id ~ ClsTmplOpt ~> Concat4 )
  def ClsTmplOpt: R1 = rule( `extends` ~ ClsTmpl ~> Concat | (`extends`.? ~> ExtractOpt ~ TmplBody ~> Concat).? ~> ExtractOpt )

  def ClsTmpl: R1 = {
    def Constr = rule( AnnotType ~ ((NotNewline ~ ArgList ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    def ClsParents = rule( Constr ~ ((`with` ~ AnnotType ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( (EarlyDefs.? ~> ExtractOpt) ~ ClsParents ~ (TmplBody.? ~> ExtractOpt) ~> Concat3 )
  }

  def EarlyDefs: R1 = {
    def EarlyDef: R1 = rule( ((Annot ~ OneNLMax ~> Concat).* ~> ConcatSeqNoDelim) ~ (Mod.* ~> ConcatSeqNoDelim) ~ (ValDef | VarDef) ~> Concat3 )
    def EarlyDefZero: R1 = rule ( (EarlyDef ~ ((Semis ~ EarlyDef ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt )
    rule( `{` ~ EarlyDefZero ~ `}` ~ `with` ~> Concat4 )
  }

  def PkgObj: R1 = rule( `package` ~ ObjDef ~> Concat )
  def PkgBlock: R1 = rule( `package` ~ QualId ~ `{` ~ (TopStatSeq.? ~> ExtractOpt) ~ `}` ~> Concat5 )
  def TopStatSeq: R1 = {
    def Tmpl: R1 = rule( ((Annot ~ OneNLMax ~> Concat).* ~> ConcatSeqNoDelim) ~ (Mod.* ~> ConcatSeqNoDelim) ~ (TraitDef | ClsDef | ObjDef) ~> Concat3 )
    def TopStat: R1 = rule( PkgBlock | PkgObj | Import | Tmpl )
    // rule( TopStat.+(SemisR0) ~> ConcatSeqSemi )
    rule {
      TopStat ~ ((Semis ~ TopStat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat
    }
  }

  def CompilationUnit: R1 = {
    def ThatPackageThingy: R1 = rule (`package` ~ QualId ~ !(WS ~ "{") ~> Concat)
    def ThatPackageThingyOne: R1 = rule ( ThatPackageThingy ~ ((Semis ~ ThatPackageThingy ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    def TopPackageSeq: R1 = rule( ThatPackageThingyOne )
    def Body: R1 = rule( TopPackageSeq ~ ((Semis ~ TopStatSeq ~> Concat).? ~> ExtractOpt) ~> Concat | TopStatSeq | capture(MATCH) )
    rule( Semis.? ~> ExtractOpt ~ Body ~ (Semis.? ~> ExtractOpt) ~ WL ~> Concat4 )
  }
}
