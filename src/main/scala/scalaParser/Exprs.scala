package scalaParser
import acyclic.file

/**
 * Created by haoyi on 11/30/14.
 */
trait Exprs extends Core with Types with Xml{

  private implicit def wspStr(s: String): R1 = rule( WL ~ capture(str(s)) ~> Concat )
  private implicit def wspCh (s: Char  ): R1 = rule( WL ~ capture(ch (s)) ~> Concat )

  def NewBody : R1
  def BlockDef: R1

  def Import: R1 = {
    def ImportExpr: R1 = rule( StableId ~ (('.' ~ (`_` | Selectors) ~> Concat).? ~> ExtractOpt) ~> Concat )
    def Selectors: R1 = rule( '{' ~ ((Selector ~ ',' ~> Concat).* ~> ConcatSeqNoDelim) ~ (Selector | `_`) ~ "}" ~> Concat4 )
    def Selector: R1 = rule( Id ~ ((`=>` ~ (Id | `_`) ~> Concat).? ~> ExtractOpt) ~> Concat )
    // !!! ',' can't be implicitly converted to have WL behind it, since it is R1.
    def ImportExprOne: R1 = rule ( ImportExpr ~ ((',' ~ ImportExpr ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( `import` ~ ImportExprOne ~> Concat )
  }

  def Ascription: R1 = rule( `:` ~ (`_*` |  Type | Annot.+ ~> ConcatSeqNoDelim) ~> Concat )

  def LambdaHead: R1 = {
    def Binding: R1 = rule( (Id | `_`) ~ ((`:` ~ Type ~> Concat).? ~> ExtractOpt) ~> Concat )
    def BindingZero: R1 = rule ( (Binding ~ ((',' ~ Binding ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt )
    def Bindings: R1 = rule( '(' ~ BindingZero ~ ')' ~> Concat3 )
    def Implicit: R1 = rule( `implicit`.? ~> ExtractOpt ~ Id ~ ((`:` ~ InfixType ~> Concat).? ~> ExtractOpt) ~> Concat3 )
    rule( (Bindings | Implicit | `_` ~ (Ascription.? ~> ExtractOpt) ~> Concat) ~ `=>` ~> Concat )
  }
  object StatCtx extends WsCtx(true)
  object ExprCtx extends WsCtx(false)
  def TypeExpr: R1 = ExprCtx.Expr
  class WsCtx(injectSemicolons: Boolean){

    def OneSemiMax: R1 = if (injectSemicolons) OneNLMax   else rule {capture(MATCH)}
    def NoSemis   : R1 = if (injectSemicolons) NotNewline else rule {capture(MATCH)}

    def Enumerators: R1 = {
      def Generator: R1 = rule( Pat1 ~ `<-` ~ Expr ~ (Guard.? ~> ExtractOpt) ~> Concat4 )
      def Assign: R1 = rule( Pat1 ~ `=` ~ Expr ~> Concat3 )
      def Enumerator: R1 = rule( Semis ~ Generator ~> Concat | optional(Semis) ~> ExtractOpt ~ Guard ~> Concat | Semis ~ Assign ~> Concat )
      rule( Generator ~ (Enumerator.* ~> ConcatSeqNoDelim) ~ WL ~> Concat3 )
    }
    def Expr: R1 = {
      def If: R1 = {
        def Else: R1 = rule( Semi.? ~> ExtractOpt ~ `else` ~ Expr ~> Concat3 )
        rule( `if` ~ '(' ~ ExprCtx.Expr ~ ')' ~ Expr ~ (Else.? ~> ExtractOpt) ~> Concat6 )
      }
      def While: R1 = rule( `while` ~ '(' ~ Expr ~ ')' ~ Expr ~> Concat5 )
      def Try: R1 = {
        def Catch: R1 = rule( `catch` ~ Expr ~> Concat )
        def Finally: R1 = rule( `finally` ~ Expr ~> Concat )
        rule( `try` ~ Expr ~ (Catch.? ~> ExtractOpt) ~ (Finally.? ~> ExtractOpt) ~> Concat4 )
      }
      def DoWhile: R1 = rule( `do` ~ Expr ~ (Semi.? ~> ExtractOpt) ~ `while` ~ '(' ~ Expr ~ ")" ~> Concat7 )

      def For: R1 = {
        def Body: R1 = rule( '(' ~ ExprCtx.Enumerators ~ ')' ~> Concat3 | '{' ~ StatCtx.Enumerators ~ '}' ~> Concat3 )
        rule( `for` ~ Body ~ (`yield`.? ~> ExtractOpt) ~ Expr ~> Concat4 )
      }
      def Throw: R1 = rule( `throw` ~ Expr ~> Concat )
      def Return: R1 = rule( `return` ~ (Expr.? ~> ExtractOpt) ~> Concat )

      def SmallerExpr: R1 = rule( PostfixExpr ~ ((`match` ~ '{' ~ CaseClauses ~ "}" ~> Concat4 | Ascription).? ~> ExtractOpt) ~> Concat )
      def LambdaBody: R1 = rule( If | While | Try | DoWhile | For | Throw | Return | SmallerExpr )
      rule( LambdaHead.* ~> ConcatSeqNoDelim ~ LambdaBody ~> Concat )
    }

    def PostfixExpr: R1 = {
      // ! negation !!!
      def Prefixed: R1 = rule( (WL ~ capture(anyOf("-+~!")) ~ WS ~ !Basic.OpChar ~> Concat3) ~  SimpleExpr ~> Concat )
      def Assign: R1 = rule( SimpleExpr ~ ((`=` ~ Expr ~> Concat).? ~> ExtractOpt) ~> Concat )
      def PrefixExpr: R1 = rule( Prefixed | Assign )

      def InfixExpr: R1 = rule( PrefixExpr ~ ((NoSemis ~ Id ~ (TypeArgs.? ~> ExtractOpt) ~ OneSemiMax ~ PrefixExpr ~> Concat5).* ~> ConcatSeqNoDelim) ~> Concat )
      rule( InfixExpr ~ ((NotNewline ~ Id ~ (Newline.? ~> ExtractOpt) ~> Concat3).? ~> ExtractOpt) ~> Concat )
    }

    def SimpleExpr: R1 = {
      def Path: R1 = rule( (Id ~ '.' ~> Concat).* ~> ConcatSeqNoDelim ~ `this` ~ (('.' ~ Id ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat3 | StableId )
      def New: R1 = rule( `new` ~ NewBody ~> Concat )
      def Parened: R1 = rule ( '(' ~ (Exprs.? ~> ExtractOpt) ~ ")" ~> Concat3  )
      def SimpleExpr1: R1 = rule( XmlExpr | New | BlockExpr | Literal | Path | `_` | Parened)
      rule( SimpleExpr1 ~ (('.' ~ Id ~> Concat | TypeArgs | NoSemis ~ ArgList ~> Concat).* ~> ConcatSeqNoDelim) ~ ((NoSemis  ~ `_` ~> Concat).? ~> ExtractOpt) ~> Concat3)
    }
    def Guard : R1 = rule( `if` ~ PostfixExpr ~> Concat )
  }
  def SimplePat: R1 = {
    def PatZero: R1 = rule ( (Pat ~ ((',' ~ Pat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat).? ~> ExtractOpt )
    def ExtractorArgs: R1 = rule( PatZero )
    def Extractor: R1 = rule( StableId ~ (('(' ~ ExtractorArgs ~ ')' ~> Concat3).? ~> ExtractOpt) ~> Concat )
    def TupleEx: R1 = rule( '(' ~ (ExtractorArgs.? ~> ExtractOpt) ~ ')' ~> Concat3 )
    def Thingy: R1 = rule( `_` ~ ((`:` ~ TypePat ~> Concat).? ~> ExtractOpt) ~ !"*" ~> Concat )  // !"*" doesn't have an effect anyway, hence Concat and not Concat3
    rule( XmlPattern | Thingy | Literal | TupleEx | Extractor | VarId)
  }

  def BlockExpr: R1 = rule( '{' ~ (CaseClauses | Block) ~ `}` ~> Concat3 )

  def BlockStats: R1 = {
    def Prelude: R1 = rule( Annot.* ~> ConcatSeqNoDelim ~ (`implicit`.? ~> ExtractOpt) ~ (`lazy`.? ~> ExtractOpt) ~ (LocalMod.* ~> ConcatSeqNoDelim) ~> Concat4 )
    def Tmpl: R1 = rule( Prelude ~ BlockDef ~> Concat )
    def BlockStat: R1 = rule( Import | Tmpl | StatCtx.Expr )
    def BlockStatOne: R1 = rule ( BlockStat ~ ((Semis ~ BlockStat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( BlockStatOne )
  }

  def Block: R1 = {
    def End: R1 = rule( Semis.? ~> ExtractOpt ~ &("}" | `case`) )
    def ResultExpr: R1 = rule{ StatCtx.Expr | LambdaHead ~ Block ~> Concat}
    def Body: R1 = rule( ResultExpr ~ End ~> Concat | BlockStats ~ ((Semis ~ ResultExpr ~> Concat).? ~> ExtractOpt) ~ End ~> Concat3 | End )
    rule( LambdaHead.* ~> ConcatSeqNoDelim ~ (Semis.? ~> ExtractOpt) ~ Body ~> Concat3 )
  }

  def Patterns: R1 = {
    def PatOne: R1 = rule ( Pat ~ (("," ~ Pat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( PatOne )
  }
  // !!! Problems might arise with pipe: whitespace before it is not likely to be permitted. In the real world, it is always almost present.
  def Pat : R1 = {
    def Pat1One: R1 = rule ( Pat1 ~ (('|' ~ Pat1 ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( Pat1One )
  }
  def Pat1: R1 = rule( `_` ~ `:` ~ TypePat ~> Concat3 | VarId ~ `:` ~ TypePat ~> Concat3 | Pat2 )
  def Pat2: R1 = {
    def Pat3 = rule( `_*` | SimplePat ~ ((Id ~ SimplePat ~> Concat).* ~> ConcatSeqNoDelim) ~> Concat )
    rule( (VarId | `_`) ~ `@` ~ Pat3 ~> Concat3 | Pat3 | VarId )
  }

  def TypePat: R1 = rule( CompoundType )

  def ArgList: R1 = rule( '(' ~ ((Exprs ~ ((`:` ~ `_*` ~> Concat).? ~> ExtractOpt) ~> Concat).? ~> ExtractOpt) ~ ")" ~> Concat3 | OneNLMax ~ BlockExpr ~> Concat )

  def CaseClauses: R1 = {
    def CaseClause: R1 = rule( `case` ~ Pat ~ (ExprCtx.Guard.? ~> ExtractOpt) ~ `=>` ~ Block ~> Concat5 )
    rule( CaseClause.+ ~> ConcatSeqNoDelim )
  }
}
