{
module Compiler.Parser.Parser (parse'module, parse'decls, parse'expr, parse'type) where

import Control.Monad (unless, fail)
import Control.Monad.State
import Data.List (concat)

import Compiler.Lexer.Token
import Compiler.Lexer.Lexer
import Compiler.Lexer.Utils
import Compiler.Lexer.LexerState

import Compiler.Parser.Utils

import Compiler.Syntax.Name
import Compiler.Syntax.Literal
import Compiler.Syntax.Fixity

import Compiler.Syntax.Term
}

%name parsermain
%name parserdecls Declaration
%name parserexpr Expression
%name parsertype Type
%tokentype { Token }
%error { parse'error }
%monad { Parser }
%lexer { lexer } { Tok'EOF _ }
-- %expect 0


%token
  data          { Tok'Data _ }
  if            { Tok'If _ }
  then          { Tok'Then _ }
  else          { Tok'Else _ }
  let           { Tok'Let _ }
  in            { Tok'In _ }
  case          { Tok'Case _ }
  of            { Tok'Of _ }
  type          { Tok'Type _ }
  '_'           { Tok'Underscore _ }
  lambda        { Tok'Lambda _ }
  class         { Tok'Class _ }
  instance      { Tok'Instance _ }
  where         { Tok'Where _ }
  module        { Tok'Module _ }
  '::'          { Tok'Has'Type _ }
  infixl        { Tok'Infixl _ }
  infix         { Tok'Infix _ }
  infixr        { Tok'Infixr _ }
  forall        { Tok'Forall _ }


  '->'          { Tok'Operator "->" _ }
  '=>'          { Tok'Operator "=>" _ }
  '='           { Tok'Operator "=" _ }
  '|'           { Tok'Operator "|" _ }
  '..'          { Tok'Operator ".." _ }
  '@'           { Tok'Operator "@" _ }


  varid         { Tok'Ident'Var $$ _ }
  conid         { Tok'Ident'Const $$ _ }
  dot           { Tok'Operator "." _ }
  op            { Tok'Operator $$ _ }
  opcon         { Tok'Operator'Const $$ _ }


  '('           { Tok'Left'Paren _ }
  ')'           { Tok'Right'Paren _ }
  '['           { Tok'Left'Bracket _ }
  ']'           { Tok'Right'Bracket _ }
  ','           { Tok'Comma _ }
  '`'           { Tok'Backtick _ }
  '{'           { Tok'Left'Brace _ }
  '}'           { Tok'Right'Brace _ }
  ';'           { Tok'Semicolon _ }


  integer       { Tok'Int $$ _ }
  char          { Tok'Char $$ _ }
  double        { Tok'Double $$ _ }
  string        { Tok'String $$ _ }


%%
Program         ::  { [Term'Decl] }
                :   Module                                          { $1 }


Module          ::  { [Term'Decl] }
                :   module conid where Layout(Declaration)          { concat $4 }


Declaration     ::  { [Term'Decl] }
                :   Data                                            { [ $1 ] }
                |   TypeSynonym                                     { [ $1 ] }
                |   FixitySigns                                     { $1 }
                |   TypeSigns                                       { $1 }
                |   ClassDecl                                       { [ $1 ] }
                |   InstanceDecl                                    { [ $1 ] }
                |   Binding                                         { [ $1 ] }


{- Data Declaration -}
Data            ::  { Term'Decl }
                :   data conid Params Constructors                { Data'Decl $2 $3 $4 }
                |   data '(' opcon ')' Params Constructors        { Data'Decl $3 $5 $6 }
                |   data '(' ')' Params Constructors              { Data'Decl "()" $4 $5 }



Constructors    ::  { [Term'Constr'Decl] }
                :   {- empty -}                                     { [] }
                |   '=' Constr NoneOrMany(ConstrOther)              { $2 : $3 }


Constr          ::  { Term'Constr'Decl }
                :   conid NoneOrMany(AType)                         { Con'Decl $1 $2 }
                |   '(' opcon ')' NoneOrMany(AType)                 { Con'Decl $2 $4 }
                
                |   '(' ')'                                         { Con'Decl "()" [] }
                |   '[' ']'                                         { Con'Decl "[]" []}

                |   BType ConInfix BType                            { Con'Decl $2 [$1, $3] }
                {- NOTE: According the Haskell report, both type operands can be either BType or "banged" AType -}
                {- since I don't have a "!" bang operator, I am not doing that here -}
                
                |   conid '{' NoneOrManySeparated(TypeField) '}'    { Con'Record'Decl $1 $3 }
                |   '(' opcon ')' '{' NoneOrManySeparated(TypeField) '}'
                                                                    { Con'Record'Decl $2 $5 }


TypeField       ::  { (String, Term'Type) }
                :   LowIdent '::' Type                              { ($1, $3) }
                {- NOTE: According the Haskell report, Type could also be "! AType" -}


ConInfix        ::  { String }
                :   opcon                                           { $1 }
                |   '`' conid '`'                                   { $2 }


ConstrOther     ::  { Term'Constr'Decl }
                :   '|' Constr                                      { $2 }


{- Type Alias/Synonym Declaration -}
TypeSynonym     ::  { Term'Decl }
                :   type conid Params '=' Type                      { Type'Alias $2 $3 $5 }


{- Identifiers -}
Params          ::  { [String] }
                :   NoneOrMany(varid)                               { $1 }


LowIdent        ::  { String }
                :   varid                                           { $1 }
                |   '(' Op ')'                                      { $2 }


-- UpIdent         ::  { String }
--                 :   conid                                           { $1 }
--                 |   '(' opcon ')'                                   { $2 }
--                 |   Unit                                            { "()" }


Unit            ::  { () }
                :   '(' ')'                                         { () }


OpInfix         ::  { Term'Id }
                :   Oper                                            { $1 }
                |   '`' varid '`'                                   { Term'Id'Var $2 }
                |   '`' conid '`'                                   { Term'Id'Const $2 }


Op              ::  { String }
                :   op                                              { $1 }
                |   dot                                             { "." }


Oper            ::  { Term'Id }
                :   Op                                              { Term'Id'Var $1 }
                |   opcon                                           { Term'Id'Const $1 }


OpInfix_        ::  { String }
                :   OpInfix                                         { case $1 of
                                                                      { Term'Id'Var name -> name
                                                                      ; Term'Id'Const name -> name }}

-- {- Fixity Signature -}
FixitySigns     ::  { [Term'Decl] }
                :   Fixity integer OneOrManySeparated(OpInfix_)
                                                                    { map (Fixity $1 $2) $3 }


Fixity          ::  { Fixity }
                :   infixl                                          { Infixl }
                |   infix                                           { Infix }
                |   infixr                                          { Infixr }


{- Type Annotation Signature -}
TypeSigns       ::  { [Term'Decl] }
                :   OneOrManySeparated(LowIdent) '::' QualType      { map (\ name -> Signature name $3) $1 }


{- Class Declaration -}
ClassDecl       ::  { Term'Decl }
                :   class ClassDecl2 ClassSigns                     { let { (ctx, name, var'name) = $2 } in Class'Decl name var'name ctx $3 }


ClassDecl2      ::  { ([Term'Pred], Name, Name) }
                :   conid varid '=>' conid varid                    { ([Is'In $1 (Term'T'Id (Term'Id'Var $2))], $4, $5) }
                |   conid varid                                     { ([], $1, $2) }
                |   '(' NoneOrManySeparated(SimpleClass) ')' '=>' conid varid
                                                                    { ($2, $5, $6) }


ClassSigns      ::  { [Term'Decl] }
                :   where Layout(TypeSigns)                         { concat $2 }
                |   {- empty -}                                     { [] }


{- Instance Declaration -}
InstanceDecl    ::  { Term'Decl }
                :  instance InstanceDecl2 InstBinds                 { let { (ctx, pred) = $2 } in Instance (ctx, pred) $3 }


InstanceDecl2   ::  { ([Term'Pred], Term'Pred) }
                :   conid varid '=>' conid Type                     { ([Is'In $1 (Term'T'Id (Term'Id'Var $2))], Is'In $4 $5) }
{- NOTE: There is a restriction on the     ^^^^ Type part -}

                |   conid Type                                      { ([], Is'In $1 $2) }
{- NOTE:                  ^^^^ -}

                |   '(' NoneOrManySeparated(SimpleClass) ')' '=>' conid Type
                                                                    { ($2, Is'In $5 $6) }
{- NOTE:                                                                ^^^^ -}
{- As described in the Haskell Report 98 -}
{-
  -> 	gtycon
	| 	( gtycon tyvar1 ... tyvark )		(k>=0, tyvars distinct)
	| 	( tyvar1 , ... , tyvark )		(k>=2, tyvars distinct)
	| 	[ tyvar ]
	| 	( tyvar1 -> tyvar2 )		(tyvar1 and tyvar2 distinct) 
-}
{- TODO: If I keep parsing it as Type, I will need to check this part during semantic analysis. -}

InstBinds       ::  { [Term'Decl] }
                :   where Layout(Binding)                           { $2 }
                |   {- empty -}                                     { [] }

-- No longer needed, has been replaced with ClassDecl2 and InstanceDecl2
-- {- Simple Contexts -}
-- SimpleContext   ::  { [Term'Pred] }
--                 :   SimpleClass '=>'                                { [$1] }
--                 |   '(' NoneOrManySeparated(SimpleClass) ')' '=>'   { $2 }
--                 |   {- no context -}                                { [] }


SimpleClass     ::  { Term'Pred }
                :   conid Type                                      { Is'In $1 $2 }
                {-  Semantic Restrictions:
                    1) $2 have to be a Type Variable
                    2) $ the actual identifier for $2 must be bound by the class/instance declaration -}


{- Binding -}
Binding         ::  { Term'Decl }
                :   FunLHS RHS                                      { Binding $1 $2 }


FunLHS          ::  { Term'Pat }
FunLHS          :   Pattern                                         { $1 }


RHS             ::  { Term'Expr }
                :   '=' Expression                                  { $2 }


{- Pattern -}
Pattern         ::  { Term'Pat }
                :   OneOrMany(BPat)                                 { case $1 of
                                                                      { [one'pattern] -> one'pattern
                                                                      ; many'patterns -> Term'P'App many'patterns } }
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
                |   '(' ')'                                         { Term'P'Id $ Term'Id'Const "()" }
                |   '[' ']'                                         { Term'P'Id $ Term'Id'Const "[]" }
                |   '(' ',' NoneOrMany(',') ')'                     { Term'P'Id $ Term'Id'Const undefined }
                {- TODO: Figure out how to insert tuple constructor for arbitraryly sized tuples -}
                -- IDEA: On Term level - let's have Tuple'Constr or something. And later on
                -- during the semantic analysis and transformation step let's collect all tuple sizes
                -- and insert corresponding type constructors to the typying environment or wherever.
                |   conid '{' NoneOrManySeparated(FPat) '}'         { Term'P'Labeled $1 $3 }
                |   Literal                                         { Term'P'Lit $1 }
                |   string                                          { Term'P'App ((Term'P'Id $ Term'Id'Const "[]") : map (Term'P'Lit . Lit'Char) $1) }
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
                |   let Layout(Declaration) in Expression           { Term'E'Let (concat $2) $4 }
                |   if Expression then Expression else Expression   { Term'E'If $2 $4 $6 }
                |   case Expression of Layout(Alt)                  { Term'E'Case $2 $4 }
                |   OneOrMany(BExp)                                 { case $1 of
                                                                      { [one'expr] -> one'expr
                                                                      ; many'exprs -> Term'E'App many'exprs } }
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
                |   string                                          { Term'E'App ((Term'E'Id $ Term'Id'Const "[]") : map (Term'E'Lit . Lit'Char) $1) }
                |   '(' Expression ')'                              { $2 }
                |   '(' Expression ',' OneOrManySeparated(Expression) ')'
                                                                    { Term'E'Tuple ($2 : $4) }
                |   '[' NoneOrManySeparated(Expression) ']'         { Term'E'List $2 }
                {-  TODO: use foldl to construct Term'App with singleton lists -}
                {-  It will be easier to find possible syntax errors like -}
                {-  `[ (+), 1 2 ]` vs `: ((+) 1 2) []` -}
                {-  It there's none, produce an empty List -}
                |   '[' Expression MaybeSeparated(Expression) '..' Expression ']'
                                                                    { Term'E'Arith'Seq $2 $3 $5 }
                |   conid '{' NoneOrManySeparated(FieldBind) '}'    { Term'E'Labeled'Constr $1 $3 }
                |   AExp '{' OneOrManySeparated(FieldBind) '}'      { case $1 of
                                                                      { Term'E'Id (Term'Id'Const name) -> Term'E'Labeled'Constr name $3
                                                                      ; _ -> Term'E'Labeled'Update $1 $3 } }
                |   '(' Oper ')'                                    { Term'E'Op $2 }
                |   '(' ')'                                         { Term'E'Id $ Term'Id'Const "()" }


FieldBind       ::  { (Name, Term'Expr) }
                :   varid '=' Expression                            { ($1, $3) }


Literal         ::  { Literal }
                :   integer                                         { Lit'Int $1 }
                |   double                                          { Lit'Double $1 }
                |   char                                            { Lit'Char $1 }
                

                -- |   string                                          { Term'E'App ((Term'E'Id $ Term'Id'Const "[]") : map (Term'E'Lit . Lit'Char) $1) }
                  
                  -- foldr (\ item acc -> App (App (Var ":") (Lit $ Lit'Char item)) acc ) (Var "[]") $1 }


{- Type / Qualified Type -}
QualType        ::  { ([Term'Pred], Term'Type) }
                :   Type MaybeArrowType   {% do case $2 of
                                            { Nothing -> return ([], $1)
                                            ; Just t -> do 
                                                { let predicates = to'predicates $1
                                                ; return (predicates, t) } } }


MaybeArrowType  ::  { Maybe Term'Type }
                :   {- empty -}   { Nothing }
                |   '=>' Type     { Just $2 }


TypeContext     ::  { [Term'Pred] }
                :   {- empty -}                                     { [] }
                |   Predicate '=>'                                  { [$1] }
                |   '(' NoneOrManySeparated(Predicate) ')' '=>'     { $2 }


Predicate       ::  { Term'Pred }
                :   conid varid                                     { Is'In $1 $ Term'T'Id $ Term'Id'Var $2 }
                |   conid '(' Type ')'                              { Is'In $1 $3 }
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


Type              ::  { Term'Type }
                  :   BType NoneOrMany(TypeArr)                     { case $2 of
                                                                        { [] -> $1
                                                                        ; (t : ts) -> Term'T'Arrow ($1 : $2) } }


TypeArr           ::  { Term'Type }
                  :   '->' Type                                     { $2 }


BType             ::  { Term'Type }
                  :   OneOrMany(AType)                              { case $1 of
                                                                      { [t] -> t
                                                                      ; (t : ts) -> case t of
                                                                        { Term'T'App types -> Term'T'App $ types ++ ts
                                                                        ; _ -> Term'T'App (t : ts) } } }
{- Explanation: If you have a type expression like: `T a b` it can be written like `(T a) b`.
    But but parsing that second form would result in creating something like: Term'T'App [Term'T'App [T, a], b].
    That is generally OK. But if I want to check that all Type Synonyms are fully applied I would be having a hard time.
    For that reason I decided to try this approach instead. That is - Type Applications of thhis specific shape
    are going to be collapsed. But only Type Applications which are a first element of another Type Application.
    That should 1:1 correspond to the well known rule of implicit parenthesizing of (not only) Type Applications.
-}


AType             ::  { Term'Type }
                  :   GTyCon                                        { $1 }
                  |   varid                                         { Term'T'Id $ Term'Id'Var $1 }
                  |   '(' Type ',' OneOrManySeparated(Type) ')'     { Term'T'Tuple ($2 : $4) }
                  |   '[' Type ']'                                  { Term'T'List $2 }
                  |   '(' Type ')'                                  { $2 }
                  |   forall OneOrMany(varid) dot QualType          { Term'T'Forall (map Term'Id'Var $2) $4 }
                  --  ^ this line introduces about 12 new shift/reduce conflicts - if I forced the forall type to be wrapped in the parens it would not



GTyCon            ::  { Term'Type }
                  :   conid                                         { Term'T'Id $ Term'Id'Const $1 }
                  |   '(' ')'                                       { Term'T'Id $ Term'Id'Const "()" }
                  |   '['  ']'                                      { Term'T'Id $ Term'Id'Const "[]" }
                  |   '(' '->' ')'                                  { Term'T'Id $ Term'Id'Const "(->)" }
                  |   '(' ',' NoneOrMany(',') ')'                   { Term'T'Id $ Term'Id'Const "(FIX, TUPLE)" }
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


Maybe(tok)
                :   {- empty -}                                     { Nothing }
                |   tok                                             { Just $1 }


{

parse'error _ = do
  l'no <- get'line'no
  col'no <- get'col'no
  state <- get
  error $ "Parse error on line " ++ show l'no ++ ", column " ++ show col'no ++ "." ++ "  " ++ show state


parse'module :: String -> [Term'Decl]
parse'module source = eval'parser parsermain source


parse'decls :: String -> [Term'Decl]
parse'decls source = eval'parser parserdecls source


parse'expr :: String -> Term'Expr
parse'expr source = eval'parser parserexpr source


parse'type :: String -> Term'Type
parse'type source = eval'parser parsertype source

}
