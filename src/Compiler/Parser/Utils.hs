module Compiler.Parser.Utils where


import Compiler.Syntax.Term.Type
import Compiler.Syntax.Term.Predicate
import Compiler.Syntax.Term.Identifier


-- |  4.1.3  Syntax of Class Assertions and Contexts
-- |  
-- |  context 	-> 	class
-- |  	        | 	( class1 , ... , classn )		(n>=0)
-- |  class 	  -> 	qtycls tyvar
-- |  	        | 	qtycls ( tyvar atype1 ... atypen ) 	(n>=1)
-- |  qtycls 	  -> 	[ modid . ] tycls
-- |  tycls 	  -> 	conid
-- |  tyvar 	  -> 	varid

-- |  A class assertion has form qtycls tyvar, and indicates the membership of the type tyvar
-- |  in the class qtycls. A class identifier begins with an uppercase letter.
-- |  A context consists of zero or more class assertions, and has the general form  ( C1 u1, ..., Cn un )
-- |  where C1, ..., Cn are class identifiers, and each of the u1, ..., un is either a type variable,
-- |  or the application of type variable to one or more types.
-- |  The outer parentheses may be omitted when n=1.
-- |  In general, we use cx to denote a context and we write cx => t to indicate the type t restricted
-- |  by the context cx. The context cx must only contain type variables referenced in t.
-- |  For convenience, we write cx => t even if the context cx is empty,
-- |  although in this case the concrete syntax contains no =>.


to'predicates :: Term'Type -> [Term'Pred]
to'predicates (Term'T'Id (Term'Id'Const "()"))
  = []
to'predicates (Term'T'Tuple types) = concatMap to'predicates types
to'predicates (Term'T'App [Term'T'Id (Term'Id'Const con), var@(Term'T'Id (Term'Id'Var _))])
  = [Is'In con var]
to'predicates (Term'T'App [Term'T'Id (Term'Id'Const con), t@(Term'T'App (Term'T'Id (Term'Id'Var _) : _ : _))  ])
  = [Is'In con t]
to'predicates _ = error "type context is not in the correct shape"
