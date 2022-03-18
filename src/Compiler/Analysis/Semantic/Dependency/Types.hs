{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analysis.Semantic.Dependency.Types where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Graph ( SCC(..), stronglyConnComp )


import Compiler.Syntax.Declaration ( Data(..), Constr'Decl(..), Class(..), Declaration(..) )
import Compiler.Syntax.Type ( T'C(..), T'V'(..), Type(..) )
import Compiler.Syntax.Qualified ( Qualified(..) )
import Compiler.Syntax.Predicate ( Predicate(..) )

import Compiler.Analysis.Semantic.Dependency.Depends ( Depends(..) )
import Compiler.Syntax.Signature ( Signature(T'Signature) )


sort :: [Either Data Class] -> [[Either Data Class]]
sort data' = sorted
  where
    indexer :: Map.Map String Int
    indexer = Map.empty

    dependencies :: [(Either Data Class, Int, Set.Set Int)]
    dependencies = depends'on data' indexer

    graph :: [(Either Data Class, Int, [Int])]
    graph = map (\ (a, b, c) -> (a, b, Set.toList c)) dependencies

    sccs :: [SCC (Either Data Class)]
    sccs = stronglyConnComp graph

    scc'to'list :: SCC (Either Data Class) -> [Either Data Class]
    scc'to'list (AcyclicSCC bg) = [bg]
    scc'to'list (CyclicSCC bgs) = bgs

    sorted :: [[Either Data Class]]
    sorted = map scc'to'list sccs


instance {-# OVERLAPPING #-} Depends [Either Data Class] [(Either Data Class, Int, Set.Set Int)] where
  depends'on data' indexer'glob
    = dependencies
      where
        index'data' :: [Either Data Class] -> Map.Map String Int
        index'data' = enumerate'types'n'constraints' $ Map.size indexer'glob


        {-  Assigns each data declaration / type constructor an unique number/index.  -}
        enumerate'types'n'constraints' :: Int -> [Either Data Class] -> Map.Map String Int
        enumerate'types'n'constraints' _ [] = Map.empty
        enumerate'types'n'constraints' n (Left Data{ type'name = T'C t'name _ } : eis)
          = Map.insert t'name n $ enumerate'types'n'constraints' (n + 1) eis
        enumerate'types'n'constraints' n (Right Class{ class'name = cl'name } : eis)
          = Map.insert cl'name n $ enumerate'types'n'constraints' (n + 1) eis


        indexer :: Map.Map String Int
        indexer = index'data' data' `Map.union` indexer'glob
        -- takes the left biased union
        -- in this case - it shouldn't matter - there are no "local" type declarations


        dependencies :: [(Either Data Class, Int, Set.Set Int)]
        dependencies = map make'dependency data'


        make'dependency :: Either Data Class -> (Either Data Class, Int, Set.Set Int)
        make'dependency d@(Left Data{ type'name = T'C t'name _, constructors = constrs })
          = (d, indexer Map.! t'name, depends'on constrs indexer)
        make'dependency c@(Right Class{ class'name = cl'name, class'supers = cl'supers, class'declarations = cl'decls })
          = (c, indexer Map.! cl'name, depends'on cl'supers indexer `Set.union` depends'on cl'decls indexer)


instance {-# OVERLAPPABLE #-} Depends a (Set.Set Int) => Depends [a] (Set.Set Int) where
  depends'on as indexer = foldl (flip $ Set.union . flip depends'on indexer) Set.empty as


instance Depends Constr'Decl (Set.Set Int) where
  depends'on (Con'Decl _ types) indexer
    = depends'on types indexer
  depends'on (Con'Record'Decl _ fields) indexer
    = Set.unions $ map (\ (_, t) -> depends'on t indexer) fields


instance Depends Type (Set.Set Int) where
  depends'on (T'Var' _) _
    = Set.empty

  depends'on (T'Meta _) _
    = Set.empty
  
  depends'on (T'Con (T'C con'name _)) indexer
    = maybe Set.empty Set.singleton (indexer Map.!? con'name)
  
  depends'on (T'Tuple types) indexer
    = depends'on types indexer
  
  depends'on (T'App t'left t'right) indexer
    = depends'on [t'left, t'right] indexer
  
  depends'on (T'Forall t'v (preds :=> type')) indexer
    = depends'on preds indexer `Set.union` depends'on type' indexer


instance Depends Predicate (Set.Set Int) where
  depends'on (Is'In class'name type') indexer
    = depends'on type' indexer `Set.union` maybe Set.empty Set.singleton (indexer Map.!? class'name)


instance Depends Declaration (Set.Set Int) where
  depends'on (Signature (T'Signature name type')) indexer
    = depends'on type' indexer
  depends'on _ _ = error "ERROR: It seems like the source contains a class declaration which contains more than just type signatures."
