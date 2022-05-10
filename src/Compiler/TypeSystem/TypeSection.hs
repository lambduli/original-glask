module Compiler.TypeSystem.TypeSection where


import Compiler.Syntax.Declaration ( Data, Class )
import Compiler.Syntax.Instance ( Instance )


type Type'Section = ([Data], [Class], [Instance])
