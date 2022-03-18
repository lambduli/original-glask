{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analysis.Semantic.Dependency.Binding where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Graph (SCC(..), stronglyConnComp)


import Compiler.Syntax.Declaration ( Declaration(Binding) )
import Compiler.Syntax.BindGroup ( Bind'Group(..) )
import Compiler.Syntax.Expression ( Expression(..) )
import Compiler.Syntax.Match ( Match(Match, rhs) )

import Compiler.Analysis.Semantic.Dependency.Depends ( Depends(..) )


{-  MOTE: This function takes a list of Declarations and sorts them in the Topological Order using Data.Graph module. -}
sort :: [Bind'Group] -> [[Bind'Group]]
sort bind'groups = sorted
  where
    indexer :: Map.Map String Int
    indexer = Map.empty

    dependencies :: [(Bind'Group, Int, Set.Set Int)]
    dependencies = depends'on bind'groups indexer

    graph :: [(Bind'Group, Int, [Int])]
    graph = map (\ (a, b, c) -> (a, b, Set.toList c)) dependencies

    sccs :: [SCC Bind'Group]
    sccs = stronglyConnComp graph

    scc'to'list :: SCC Bind'Group -> [Bind'Group]
    scc'to'list (AcyclicSCC bg) = [bg]
    scc'to'list (CyclicSCC bgs) = bgs

    sorted :: [[Bind'Group]]
    sorted = map scc'to'list sccs



-- takze implementacne
-- budu potrebovat funkci, co umi prolezt vsechny Expression a posbirat promenny
-- budu muset prochazet i Patterny?
-- jelikoz Patterny nemuzou referencovat promenny zvenku (jenom je vytvari) tak to nebude problem
-- Patterny muzou ale referencovat konstruktory, ty jsou ale explicitne otypovany, takze ty vubec
-- nebudu brat pro dependency analysis v potaz - ty tam proste jsou uz od zacatku pro vsechny


-- | ZASADNI (2) OTAZKY JSOU
-- 1) co udelat s Type Signatures -- mel bych je brat v potaz pri vypoctu zavislosti?
--
-- 2) co udelat s class memberama? -- mel bych kazdemu memberu udelit jedno cislo
-- podle deklarace?
-- jenze tady mi prijde problem, ze cil je sestavit graf pro inferenci zejo
-- ale ono tech IMPLEMENTACI metody nebo memberu obecne
-- bude vic a kazda z nich bude dependent na necem jinem
-- takze neni pak duvod, aby vsechny byly brany jako ta sama vec
-- nektera metoda nebude vubec zminovat
-- odpoved - od toho jsou class members explicitne anotovany - abys vedel, jakej typ to bude
-- nechal si to (asi na konec - nevim jiste - mrknu do THIH), infernul vsechno ostatni
-- a pak spolu s ostatnima anotovanyma vecma infernul i class/instance members


--
-- kdyz jde o top level deklarace
-- tak nemam zadny indexer
-- sestavim si indexer a na lokalni lety nahlizim takhle:
  -- cokoliv co je deklarovany lokalnim letem me nezajima z hlediska globalnich deklaraci

-- takze bych to mozna mohl udelat tak, ze by vsechny depends'on brali indexer jako parametr
-- a kdyz se dojde k lokalnim letum
-- tak se musi vytvorit novy indexer - lokalni
-- resp - on se nemusi vytvorit celej novej indexer, staci ze se posbiraji deklarace v letu
-- zacne se ale pocitat s nkem ktery je nasledujici klic v indexeru (soucasnem)
-- a pak se to do nej prida
-- timhle vlastne simulujes scopy - lokalni hodnoty prepisou ty globalni
-- kdyz se pak narazi na nejaky identifikator promenne
-- tak se najde spravna ciselna hodnota
-- a kdyby se nahodou stalo, ze v dependencies bude cislo, ktery je z lokalniho scopu
-- coz by se stat nemelo, PROTOZE kdyz budu ukladat cisla do dependencies
-- tak vzdycky do toho "globalniho" ulozim jenom to, co patri do meho globalniho scopu
-- jak to ale poznam?
-- dostanu set hodnot
-- a projdu je - a vyfiltruju vsechny hodnoty vetsi a rovno, nez je velikost meho
-- soucasneho "globalniho" indexeru
-- 
-- takze kdyz se bude volat depends'on pro [Declaration]
-- tak se ven dostane vsechno to "globalni"
-- ale pokud se to bude volat v miste lokalniho scopu, tak se zahodi zavislosti na vsechny
-- identifikatory, ktery pochazeji z lokalniho scopu

-- ted si muzes polozit otazku
-- proc mi nemuze ten lokalni [Declaration] analysis vysledek rovnou davat
-- jenom ty globalni zavislosti skrz to
-- ze nedostane indexer obsahujici lokalni veci (a co on nema v indexeru, tak to ignoruje)
-- 
-- ale to je prave o tom, kdo ma na starost sestavit ten indexer
-- ja bych radsi, aby ho sestavovala ta cast implementace
-- ktera se stara o implementaci instance
-- Depends nad [Declaration]
-- coz ale na druhou stranu bude malinko problematicky,
-- protoze to znamena ze v pripade ze je to instance Depends [Declaration]
-- tak budu chtit udelat nejakej specialni book keeping
-- ale v pripade ze je to obecne instance Depends [a]
-- tak budu chtit aby se to chovalo uplne genericky
-- to se mi bude overlappovat
-- to asi muze vyresit pragma

-- no a nebo to vyresim tak, ze budou dostavat ten spravnej indexer uz z vyssi urovne abstrakce
-- a tenhle problem tim zanikne
-- a vyresi to i tu situaci s filtrovanim lokalnich zavislosti
-- jenom to zpusobi escape logiky `Depends on` do vyssi urovne
-- resp leakne impl. detail o tom, ze je potreba zindexovat lokalni bindingy a sestavit z nich Map

{- QUESTION:  Why is this function never used anywhere? -}
group'declarations :: [Declaration] -> [SCC Bind'Group]
group'declarations decls = sccs
  where
    -- TODO
    -- TODO
    -- TODO
    --
    --                  READ THIS:
    --
    --
    -- a opet jsem se dostal do situace, kde se zda, ze budu potrebovat leaknout indexer o level vejs
    -- aka - DRIV nez se depends'on zavola
    -- abych mohl ziskat informaci o tom, JAKY INDEX jsem priradil ke konkretni deklaraci
    -- pravda ale je, ze bych v klidu mohl tohle nechat uvnitr
    -- vracel bych ne Set.Set Int ale dvojici - jak ten set, tak i index Int
    -- nesouci informaci o tom, jaky index ma tato konkretni deklarace
    -- 

    -- a tohle by me vyresilo i ten lokalni let a where problem ne?
    -- protoze kdyz vratim [(Set.Set Int, Int)]
    -- tak pak muzu zrekonstruovat ten indexer spravne a zarucene
    -- porad to bude trosku awkward, ale zabranim leakum implementacnich detailu jak to nejvic pujde
    
    -- ne jeste to o stupen posunu a nebudu nad listem fungovat jako map
    -- ale budu vracet [(Declaration, Int, Set.Set Int,)] -- nebo tak neco
    -- a tim si vyresim problem uplne
    -- to by melo vyresit vsechny problemy a nemusel bych ani rekonstruovat lokalni indexer

    -- tohle teda ale taky znamena, ze pro lokalni let nebo where
    -- dostanu tenhle list trojic a budu muset profiltrovat ty sety v nem
    -- proto, abych smazal dependencies, ktery jsou na lokalni deklarace
    -- takze pro kazdej prvek listu, a pro kazdej prvek toho setu, to budu muset udelat
    -- to docela roste slozitost
    --
    -- So the plan is:
    -- call many'depend'on on the list of declarations
    -- get [(Declaration, Int, Set.Set Int)]
    -- because this is top level -> no filtering
    -- I only map the last element of the tuple (the Set.Set Int) to [Int]
    -- that can be directly passed to stronglyConnComp function
    --
    indexer :: Map.Map String Int
    indexer = Map.empty

    
    dependencies :: [(Bind'Group, Int, Set.Set Int)]
    dependencies = depends'on bind'groups indexer

    graph :: [(Bind'Group, Int, [Int])]
    graph = map (\ (a, b, c) -> (a, b, Set.toList c)) dependencies

    
    sccs :: [SCC Bind'Group]
    sccs = stronglyConnComp graph

    -- TODO: the following functions are a subject to a code duplication, they also work with some assumptions
    --        it is best explained in the Let section in this file
    only'binds :: [Declaration]
    only'binds = filter is'binding decls

    bind'groups :: [Bind'Group]
    bind'groups = map binding'to'bind'group only'binds

    is'binding :: Declaration -> Bool
    is'binding (Binding _) = True
    is'binding _ = False

    binding'to'bind'group :: Declaration -> Bind'Group
    binding'to'bind'group (Binding b'group) = b'group
    binding'to'bind'group _ = error "Internal Error - Impossible case happened. Check a class method `depends'on`."
    -- If that error ever occurs - it means that the filtering wasn't correct or was skipped.
    -- only'binds is supposed to always only contain list of Bindings


instance {-# OVERLAPPING #-} Depends [Bind'Group] [(Bind'Group, Int, Set.Set Int)] where
  depends'on bind'groups indexer'glob
    = dependencies
      where
        -- only'binds :: [Declaration]
        -- only'binds = filter is'binding decls


        -- only'bind'groups :: [Bind'Group]
        -- only'bind'groups = map binding'to'bind'group only'binds


        -- is'binding :: Declaration -> Bool
        -- is'binding (Binding _) = True
        -- is'binding _ = False

        -- binding'to'bind'group :: Declaration -> Bind'Group
        -- binding'to'bind'group (Binding b'group) = b'group
        -- binding'to'bind'group _ = error "Internal Error - Impossible case happened. Check a class method `depends'on`."
        -- -- If that error ever occurs - it means that the filtering wasn't correct or was skipped.
        -- -- only'binds is supposed to always only contain list of Bindings


        index'bindings :: [Bind'Group] -> Map.Map String Int
        index'bindings = enumerate'bindings $ Map.size indexer'glob


        enumerate'bindings :: Int -> [Bind'Group] -> Map.Map String Int
        enumerate'bindings _ [] = Map.empty
        enumerate'bindings n (Bind'Group name _ : bs)
          = Map.insert name n $ enumerate'bindings (n + 1) bs


        indexer :: Map.Map String Int
        indexer = index'bindings bind'groups `Map.union` indexer'glob
        -- takes the left biased union
        -- that means we prefer the local binding of the variable


        dependencies :: [(Bind'Group, Int, Set.Set Int)]
        dependencies = map make'dependency bind'groups


        make'dependency :: Bind'Group -> (Bind'Group, Int, Set.Set Int)
        make'dependency b'g@Bind'Group{ name = name, alternatives = alts }
          = (b'g, indexer Map.! name, depends'on alts indexer)


instance {-# OVERLAPPABLE #-} Depends a (Set.Set Int) => Depends [a] (Set.Set Int) where
  depends'on as indexer = foldl (flip $ Set.union . flip depends'on indexer) Set.empty as


instance Depends Bind'Group (Set.Set Int) where
  depends'on Bind'Group { alternatives = alts } indexer
    = Set.unions $ map (`depends'on` indexer) alts
    -- NOTE: Match Group might depend on itself - that's OK, we don't mind.


instance Depends Match (Set.Set Int) where
  depends'on Match { rhs = expr } indexer
    = depends'on expr indexer


instance Depends Expression (Set.Set Int) where
  depends'on (Var var'name) indexer
    = maybe Set.empty Set.singleton (indexer Map.!? var'name)

  depends'on (Const const'name) indexer
    = maybe Set.empty Set.singleton (indexer Map.!? const'name)

  depends'on (Op op'name) indexer
    = maybe Set.empty Set.singleton (indexer Map.!? op'name)

  depends'on (Lit lit) indexer
    = Set.empty

  depends'on (Hole _) _
    = Set.empty

  depends'on (Abs param'pattern body) indexer
    = depends'on body indexer

  depends'on (App left right) indexer
    = depends'on left indexer `Set.union` depends'on right indexer

  depends'on (Infix'App left oper right) indexer
    = depends'on left indexer `Set.union` depends'on oper indexer `Set.union` depends'on right indexer

  depends'on (Tuple exprs) indexer
    = depends'on exprs indexer
    -- = foldl (flip $ Set.union . flip depends'on indexer) Set.empty exprs
    -- = foldl (\ deps'acc expr -> deps'acc `Set.union` depends'on expr indexer) Set.empty exprs

  depends'on (If cond then' else') indexer
    = depends'on [cond, then', else'] indexer
    -- = let c'deps = depends'on cond
    --       t'deps = depends'on then'
    --       e'deps = depends'on else'
    --   in c'deps `Set.union` t'deps `Set.union` e'deps

  depends'on (Let decls body) indexer
    = let
        decls'deps :: [(Bind'Group, Int, Set.Set Int)]
        decls'deps = depends'on bind'groups indexer
        {-  ^ computes dependencies inside the local declaration definitions
            the result contains references for both current scope (indexer) and local scope
            (the indexer hidden inside the depends'on {which should be implemented as a method
            of the instance Depends [Declaration]})
        -}
        decls'non'local'deps :: [(Bind'Group, Int, Set.Set Int)]
        decls'non'local'deps
          = map (\ (b'g, node, deps) -> (b'g, node, Set.filter ( < Map.size indexer) deps)) decls'deps
          -- Set.filter ( < Map.size indexer) decls'deps
        {-  ^ only filter those dependencies which reference variables from the current scope
            if it has a number higher or equal to the size of current indexer -> it belongs
            to the nested scope hidden inside the depends'on call - those are not useful now
        -}
        body'deps :: Set.Set Int
        body'deps  = depends'on body $ make'loc'indexer decls'deps
        {-  ^ I had to reconstruct the local indexer created by the depends'on call at the first
            let binding. Because the body of the let is in the same scope as the declarations.
        -}
        all'combined :: Set.Set Int
        all'combined = foldl (\ set'acc (_, _, set) -> set'acc `Set.union` set) body'deps decls'non'local'deps
        {-  ^ Here I only insert all the global dependencies from the local scopes 
            to the result of depends'on called on body to combine all the dependencies together. -}
      in all'combined
      where
        -- NOTE: Think about what is happening here
        --        Right now, I am working wiht an assumption, that this module is going to be only used to analyze dependencies of expressions
        --        so I am ignoring local fixity, type and so on declarations
        --        I think that is a good strategy - first :- some of those can not be locally declared, second :- if they should be explicitly ignored, this level is the correct place
        --        it, however, leads to some code duplication, this explicit filtering of local declarations needs to happen here and on the top level too
        --        strictly speaking - if I was willing to rely on the correctness of the preceding part of the pipeline, I could just assume that there are no forbidden local declarations
        --        I think it's better not to. I may want to allow local fixity declarations, classes, type synonyms, ...
        only'binds :: [Declaration]
        only'binds = filter is'binding decls

        bind'groups :: [Bind'Group]
        bind'groups = map binding'to'bind'group only'binds

        is'binding :: Declaration -> Bool
        is'binding (Binding _) = True
        is'binding _ = False

        binding'to'bind'group :: Declaration -> Bind'Group
        binding'to'bind'group (Binding b'group) = b'group
        binding'to'bind'group _ = error "Internal Error - Impossible case happened. Check a class method `depends'on`."
        -- If that error ever occurs - it means that the filtering wasn't correct or was skipped.
        -- only'binds is supposed to always only contain list of Bindings


        make'loc'indexer :: [(Bind'Group, Int, Set.Set Int)] -> Map.Map String Int
        make'loc'indexer deps = Map.fromList indexes
          where
            indexes :: [(String, Int)]
            indexes = map (\ (Bind'Group{ name = name }, indx, _) -> (name, indx)) deps


  depends'on (Ann expr _) indexer
    = depends'on expr indexer

  depends'on (Case expr alts) indexer = combined'deps
    where
      expr'deps = depends'on expr indexer

      alts'deps = depends'on alts indexer

      combined'deps = expr'deps `Set.union` alts'deps

  -- -- This should always produce an empty Set. But just to be sure.
  -- depends'on (Intro name exprs) indexer
    -- = depends'on exprs indexer
