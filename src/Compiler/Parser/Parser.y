{
module Compiler.Parser.Parser (parse'expr, parse'type) where

import Control.Monad (unless, fail)
import Control.Monad.State
import Data.List (concat)

import Compiler.Lexer.Token
import Compiler.Lexer.Lexer
import Compiler.Lexer.Utils

import Compiler.Syntax
}

%name parsermain
%name parsertype Type
%tokentype { Token }
%error { parse'error }
%monad { Parser }
%lexer { lexer } { Tok'EOF }
-- %expect 0


%token
  data          { Tok'Data $$ }
  if            { Tok'If $$ }
  then          { Tok'Then $$ }
  else          { Tok'Else $$ }
  let           { Tok'Let $$ }
  in            { Tok'In $$ }
  case          { Tok'Case $$ }
  of            { Tok'Of $$ }
  type          { Tok'Type $$ }
  '_'           { Tok'Hole $$ }
  lambda        { Tok'Lambda $$ }
  class         { Tok'Class $$ }
  instance      { Tok'Instance $$ }
  where         { Tok'Where $$ }
  module        { Tok'Module $$ }
  '::'          { Tok'Has'Type $$ }
  infixl        { Tok'Infixl $$ }
  infix         { Tok'Infix $$ }
  infixr        { Tok'Infixr $$ }


  '->'          { Tok'Operator "->" $$ }
  '=>'          { Tok'Operator "=>" $$ }
  '='           { Tok'Operator "=" $$ }
  '|'           { Tok'Operator "|" $$ }
  '..'          { Tok'Operator ".." $$ }
  '@'           { Tok'Operator "@" $$ }


  varid         { TokVarLower $$ }
  conid         { TokVarUpper $$ }
  op            { TokOperator $$ }
  opcon         { TokOpConstr $$ }


  '('           { Tok'Left'Paren $$ }
  ')'           { Tok'Right'Paren $$ }
  '['           { Tok'Left'Bracket $$ }
  ']'           { Tok'Right'Bracket $$ }
  ','           { Tok'Comma $$ }
  '`'           { Tok'Backtick $$ }
  '{'           { Tok'Left'Brace $$ }
  '}'           { Tok'Right'Brace $$ }
  ';'           { Tok'Semicolon $$ }


--  unit          { TokVarUpper "()" } TODO: Unit will be parsed by the Parser as `'(' ')'`
  integer       { TokInt $$ }
  char          { TokChar $$ }
  double        { TokDouble $$ }
  string        { TokString $$ }


%%
Program         ::  { [Declaration] }
                :   Module                                          { $1 }


Module          ::  { [Declaration] }
                :   module conid where Layout(Declaration)          { $4 }


Declaration     ::  { [Declaration] }
                :   Data                                            { [ $1 ] }
                |   TypeSynonym                                     { [ $1 ] }
                |   FixitySigns                                     { $1 }
                |   TypeSigns                                       { $1 }
                |   ClassDecl                                       { $1 }
                |   InstanceDecl                                    { $1 }
                |   Binding                                         { [ $1 ] }


{- Data Declaration -}
Data            ::  { Declaration }
                :   data UpIdent Params Constructors                { DataDecl $2 $3 $4 }


Constructors    ::  { [Constr'Decl] }
                :   {- empty -}                                     { [] }
                |   '=' Constr NoneOrMany(ConstrOther)              { $2 : $3 }


Constr          ::  { ConstrDecl }
                :   UpIdent NoneOrMany(AType)                       { Con'Decl $1 $2 }
                |   BType ConInfix BType                            { Con'Decl $2 [$1, $3] }
                {- NOTE: According the Haskell report, both type operands can be either BType or "banged" AType -}
                {- since I don't have a "!" bang operator, I am not doing that here -}
                |   UpIdent '{' NoneOrManySeparated(TypeField) '}'  { Con'Record'Decl $1 $3 }


TypeField       ::  { (String, Type) }
                :   LowIdent '::' Type                              { ($1, $3) }
                {- NOTE: According the Haskell report, Type could also be "! AType" -}


ConInfix        ::  { String }
                :   opcon                                           { $1 }
                |   '`' conid '`'                                   { $2 }


ConstrOther     ::  { ConstrDecl }
                :   '|' Constr                                      { $2 }


{- Type Alias/Synonym Declaration -}
TypeSynonym     ::  { Declaration }
                :   type conid Params '=' Type                      { Type'Alias $2 (foldr TyOp $5 $3) }


{- Identifiers -}
Params          ::  { [String] }
                :   NoneOrMany(varid)                               { $1 }


LowIdent        ::  { String }
                :   varid                                           { $1 }
                |   '(' op ')'                                      { $2 }


UpIdent         ::  { String }
                :   conid                                           { $1 }
                |   '(' opcon ')'                                   { $2 }
                |   Unit                                            { "()" }


Unit            ::  { () }
                :   '(' ')'                                         { () }


OpInfix         ::  { Term'Id }
                :   Oper                                            { $1 }
                |   '`' varid '`'                                   { Term'Id'Var $2 }
                |   '`' conid '`'                                   { Term'Id'Const $2 }


Oper            ::  { Term'Id }
                :   op                                              { Term'Id'Var $1 }
                |   opcon                                           { Term'Id'Const $1 }


-- {- Fixity Signature -}
FixitySigns     ::  { [Declaration] }
                :   Fixity integer OneOrManySeparated(OpInfix)
                                                                    { map (Fixity $1 $2) $3 }


Fixity          ::  { Fixity }
                :   infixl                                          { Infixl }
                |   infix                                           { Infix }
                |   infixr                                          { Infixr }


{- Type Annotation Signature -}
TypeSigns       ::  { [Declaration] }
                :   OneOrManySeparated(LowIdent) '::' QualType      { map (\ name -> Signature $ T'Signature name $3) $1 }


{- Class Declaration -}
ClassDecl       ::  { Declaration }
                :   class SimpleContext conid varid ClassSigns      { Class $3 $4 $2 $5 }


ClassSigns      ::  { [Declaration] }
                :   where Layout(TypeSigns)                         { concat $2 }
                |   {- empty -}                                     { [] }


{- Instance Declaration -}
InstanceDecl    ::  { Declaration }
                :   instance SimpleContext conid Type InstBinds     { Instance ($2 :=> IsIn $3 $4) $5 }
{- NOTE: There is a integrity restriction on the ^^^^ Type part -}
{- As described in the Haskell Report 98 -}
{-
  -> 	gtycon
	| 	( gtycon tyvar1 ... tyvark )		(k>=0, tyvars distinct)
	| 	( tyvar1 , ... , tyvark )		(k>=2, tyvars distinct)
	| 	[ tyvar ]
	| 	( tyvar1 -> tyvar2 )		(tyvar1 and tyvar2 distinct) 
-}
{- TODO: If I keep parsing it as Type, I will need to check this part during semantic analysis. -}


InstBinds       ::  { [Declaration] }
                :   where Layout(Binding)                           { $2 }
                |   {- empty -}                                     { [] }


{- Simple Contexts -}
SimpleContext   ::  { [Predicate] }
                :   SimpleClass '=>'                                { [$1] }
                |   '(' NoneOrManySeparated(SimpleClass) ')' '=>'   { $2 }
                |   {- no context -}                             { [] }


SimpleClass     ::  { Predicate }
                :   conid Type                                      { IsIn $1 $2 }
                {-  Semantic Restrictions:
                    1) $2 have to be a Type Variable
                    2) $ the actual identifier for $2 must be bound by the class/instance declaration -}


{- Binding -}
Binding         ::  { Declaration }
                :   FunLHS RHS                                      { Binding $ Left ($1, $2) }


FunLHS          ::  { Term'Pat }
FunLHS          :   Pattern                                         { $1 }


RHS             ::  { Term'Expr }
                :   '=' Expression                                  { $2 }


{- Pattern -}
Pattern         ::  { Term'Pat }
                :   OneOrMany(BPat)                                 { Term'P'App $1 }
--  A sequence of "A Pattern", "Constructor", "Constructor as infix operator" or "Operator Constructor".
--  Later the whole sequence needs to be processed and according the precedence, fixity and associativity rules ->
--  transformed into tree shaped structure.


BPat            ::  { Term'Pat }
                :   conid                                           { Term'P'Id $ Term'Id'Const $1 }
                |   '`' conid '`'                                   { Term'P'Op $ Term'Id'Const $2 }
                |   opcon                                           { Term'P'Op $ Term'Id'Const $1 }
                |   '(' opcon ')'                                   { Term'P'Id $ Term'Id'Const $2 }
                |   APat                                            { $1 }


APat            ::  { Term'Pat }
                :   varid MaybeAsPat                                { case $2 of
                                                                      { Nothing -> Term'P'Id $ Term'Id'Var $1
                                                                      ; Just pat -> Term'P'As $1 pat } }
                |   Unit                                            { Term'P'Id $ Term'Id'Const "()" }
                |   '[' ']'                                         { Term'P'Id $ Term'Id'Const "[]" }
                |   '(' ',' NoneOrMany(',') ')'                     { Term'P'Id $ Term'Id'Const undefined }
                {- TODO: Figure out how to insert tuple constructor for arbitraryly sized tuples -}
                -- IDEA: On Term level - let's have Tuple'Constr or something. And later on
                -- during the semantic analysis and transformation step let's collect all tuple sizes
                -- and insert corresponding type constructors to the typying environment or wherever.
                |   conid '{' NoneOrManySeparated(FPat) '}'         { Term'P'Labeled $1 $3 }
                |   Literal                                         { Term'P'Lit $1 }
                |   '_'                                             { Term'P'Wild }
                |   '(' Pattern ')'                                 { $2 }
                |   '(' Pattern ',' OneOrManySeparated(Pattern) ')' { Term'P'Tuple ($2 : $4) }
                |   '[' OneOrManySeparated(Pattern) ']'             { Term'P'List $2 }
                {-  TODO: I can transform last two of them into pattern applications. -}


MaybeAsPat      ::  { Maybe Term'Pat }
                :   {- empty -}                                     { Nothing }
                |   '@' APat                                        { Just $2 }


FPat            ::  { (Name, Term'Pat) }
                :   varid '=' Pattern                               { ($1, $3) }


{- Expression -}
Expression      ::  { Term'Expr }
                :   Exp10 '::' QualType                             { Term'E'Ann $1 $3 }
                |   Exp10                                           { $1 }
                

Exp10           ::  { Term'Expr }
                :   lambda APat NoneOrMany(APat) '->' Expression    { foldr Term'E'Abst $5 ($2 : $3) }
                |   let Layout(Declaration) in Expression           { Term'E'Let $2 $4 }
                |   if Expression then Expression else Expression   { Term'E'If $2 $4 $5 }
                |   case Expression of Layout(Alt)                  { Term'E'Case $2 $4 }
                |   OneOrMany(BExp)                                 { Term'E'App $1 }
--  A sequence of infix operators and constructors or AExp.
--  Same as with BPat -> I will need to "straighten" them later.


Alt             ::  { (Term'Pat, Term'Expr) }
                :   Pattern '->' Expression                         { ($1, $3) }


BExp            ::  { Term'Expr }
                :   OpInfix                                         { Term'E'Op $1 }
                |   AExp                                            { $1 }
                

AExp            ::  { Term'Expr }
                :   varid                                           { Term'E'Id $ Term'Id'Var $1 }
                |   conid                                           { Term'E'Id $ Term'Id'Const $1 }
                |   Literal                                         { Term'E'Lit $1 }
                |   '(' Expression ')'                              { $2 }
                |   '(' Expression ',' OneOrManySeparated(Expression) ')'
                                                                    { Term'E'Tuple $2 : $4 }
                |   '[' NoneOrManySeparated(Expression) ']'         { Term'E'List $2 }
                {-  TODO: use foldl to construct Term'App with singleton lists -}
                {-  It will be easier to find possible syntax errors like -}
                {-  `[ (+), 1 2 ]` vs `: ((+) 1 2) []` -}
                {-  It there's none, produce an empty List -}
                |   '[' Expression MaybeSeparated(Expression) '..' Expression ']'
                                                                    { Term'E'Arith'Seq $2 $3 $5 }
                |   conid '{' NoneOrManySeparated(FieldBind) '}'    { Term'E'Labeled'Constr (Term'Id'Const $1) $3 }
                |   AExp '{' OneOrManySeparated(FieldBind) '}'      { case $1 of
                                                                      { Term'E'Id id -> Term'E'Labeled'Constr id $3
                                                                      ; _ -> Term'Labeled'Update $1 $3 } }
                |   '(' Oper ')'                                    { Term'E'Op $2 }


FieldBind       ::  { (Name, Term'Expr) }
                :   varid '=' Expression                            { ($1, $3) }


Literal         ::  { Literal }
                :   integer                                         { Lit'Int $1 }
                |   double                                          { Lit'Double $1 }
                |   char                                            { Lit'Char $1 }
                |   string                                          { foldr (\ item acc -> App (App (Var ":") (Lit $ Lit'Char item)) acc ) (Var "[]") $1 }


{- Type / Qualified Type -}
QualType        ::  { Qualified Type }
                :   TypeContext Type                                { $1 :=> $2 }


TypeContext     ::  { [Predicate] }
                :   {- empty -}                                     { [] }
                |   Predicate '=>'                                  { [$1] }
                |   '(' NoneOrManySeparated(Predicate) ')' '=>'     { $2 }


Predicate       ::  { Predicate }
                :   conid varid                                     { IsIn $1 $ T'Var $ T'V $2 }
                |   conid '(' Type ')'                              { IsIn $1 $3 }
                {-  Semantic Constraint: Type must be a Type Application.
                    Where a Type Variable is applied to at least one Type Constants
                    or their application  -}


{- Type -}
{-  NOTE: I need to add kind to the Type Variable,
--                 but since I can't possible know it at this moment,
--                 I need to generate a fresh kind variable - for that I need to make this rule monadic.
--                 -}
--                 {-  But there's small problem. If every distinct occurence of a type variable or constant
--                     gets assigned a fresh kind variable, then two instances of the same thing won't be considered
--                     equal. This needs to be fixed.
--                     - One option is to fix it here somehow. Introduce some "scoping" to the parser.
--                       But that seems to smell of leaking abstraction.
--                     - Another option is to construct something else than Type. Maybe Term'Type.
--                       It won't contain the kind and will be very simple parse representation of the Type.
--                       Then later when doing transformations, Term'Type will be transformed into Type.
--                       And because all the information is already will be there - the whole program will
--                       be parsed - I can easily distribute Kinds correctly.
--                       Down side of this approach is that I would have to refactor Declaration too.
--                       It could not contain any Type value, that also means no Predicate and no Qualified.
--                       I would need to model the same abstraction twice. Once as Type, Qualified, Predicate ...
--                       and then the "simplified" version, which would probably be keeping all the stuff
--                       in either tuples and lists or it would just mimic the final structures but with different names.
--                       Either way, it seems little un-ergonomic.
--                     - Last option I can think of would be to keep doing what I am doing now.
--                       And then in the later stage - when transforming - I can fix all up.
--                       I will have temporarily broken data, but if I do everything right, it will be
--                       very cheap and non obtrusive solution.
--                       Possible downside is - since I don't reflect the "correctness" of the data
--                       on the type level I could easily introduce some bugs.
--                       
--                     - THE VERY LAST option would be to again think about - if I really need to keep Kind
--                       in the type variables and constants. Maybe I can do without them.
--                       Maybe I can either not normalize type variable names after the solution. Or I can
--                       get the normalizing substitution and also normalize the kind context so that I can
--                       later just lookup kind of the type variable by it's type name. -}


Type              ::  { Type }
                  :   BType NoneOrMany(TypeArr)                     { case $2 of
                                                                        { [] -> $1
                                                                        ; (t : ts) ->
                                                                          let
                                                                            state = last $ t :| ts
                                                                          in foldr (-->) state ($1 : $2) } }
--                 {- TODO: implement (-->) operator -}


TypeArr           ::  { Type }
                  :   '->' Type                                     { $2 }


BType             ::  { Type }
                  :   NoneOrMany(BType) AType                       { case $1 of
                                                                        { [] -> $2
                                                                        ; (t : ts) -> T'App (foldl' T'App t ts) $2 } }


AType             ::  { Type }
                  :   GTyCon                                        { $1 }
                  |   varid                                         {%  do
                                                                        { name <- fresh'ident
                                                                        ; return $ T'Var (T'V $1 (K'V name)) } }
                  |   '(' Type ',' OneOrManySeparated(Type) ')'     { T'Tuple ($1 : $4) }
                  |   '[' Type ']'                                  { t'list $2 }
                  |   '(' Type ')'                                  { $2 }


GTyCon            ::  { Type }
                  :   conid                                         {%  do
                                                                        { name <- fresh'ident
                                                                        ; return $ T'Con (T'C $1 (K'V name)) } }
                  |   Unit                                          { T'Con $ T'C "()" }
                  |   '['  ']'                                      { T'Con $ T'C "[]" }
                  |   '(' '->' ')'                                  { T'Con $ T'C "(->)" }
                  |   '(' ',' NoneOrMany(',') ')'                   { T'Con $ T'C "TUPLE FIX" }
{-                TODO: figure out the Tuple types -}



{- Utils / Helpers -}
NoneOrMany(tok)
                :   {- empty -}                                     { [] }
                |   tok NoneOrMany(tok)                             { $1 : $2 }

Layout(tok)
                :   '{' LayoutInside(tok)                           { $2 }

LayoutInside(tok)
                :   tok '}'                                         { [$1] }
                |   tok ';' LayoutInside(tok)                       { $1 : $3 }

OneOrMany(tok)
                :   tok NoneOrMany(tok)                             { $1 : $2 }

CommaSeparated(tok)
                :   ',' tok                                         { [$2] }
                |   ',' tok CommaSeparated(tok)                     { $2 : $3 }

NoneOrManySeparated(tok)
                :   {- empty -}                                     { [] }
                |   tok                                             { [$1] }
                |   tok ',' NoneOrManySeparated(tok)                { $1 : $3 }

OneOrManySeparated(tok)
                :   tok                                             { [$1] }
                |   tok ',' OneOrManySeparated(tok)                 { $1 : $3 }

MaybeSeparated(tok)
                :   {- empty -}                                     { Nothing }
                |   ',' tok                                         { Just $2 }


{

parse'error _ = do
  l'no <- get'line'no
  col'no <- get'col'no
  state <- get
  error $ "Parse error on line " ++ show l'no ++ ", column " ++ show col'no ++ "." ++ "  " ++ show state


parse'expr :: String -> Either [Declaration] Expression
parse'expr source = eval'parser parsermain source


parse'type :: String -> Type
parse'type source = eval'parser parsertype source

}
