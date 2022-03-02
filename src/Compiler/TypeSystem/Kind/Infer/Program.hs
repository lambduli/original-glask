module Compiler.TypeSystem.Kind.Infer.Program where

import Control.Monad.Extra ( concatMapM )
import qualified Data.Map.Strict as Map
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax ( Predicate(Is'In), Constr'Decl (Con'Decl, Con'Record'Decl) )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(constraint'env), Kind'Env, Constraint'Env )

import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )

import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'type'section, infer'seq )

import Compiler.TypeSystem.Error ( Error(..) )

-- tenhle modul dostane [[Either Data Class]] serazeny top sortem
-- kazdou skupinku musim ohandlovat zvlast
-- zacne to zaregistrovanim dummy kind variables kazdymu typu a classe
-- to uz z casti je - vsechny typovy konstanty uz maji v sobe Kind
-- zbyva to provest pro Classy
-- teoreticky muzu v tomhle miste predstirat, ze Classy jsou "taky typy"
-- stejne by nemel existovat typovy konstruktor se jmenem jaky ma uz nejaka class
-- 
-- otazka je - ocekava se ode me, ze vyrobim nejaky environment nebo tak neco?
-- 
-- napada me, ze to mozna neni nutny pokud jde ciste o typy (data)
-- tam by asi stacilo pokazdy vzit tu substituci a aplikovat ji na celej zbytek toho SCC listu
-- ale co s tridama?
-- 

-- na konci bych ale taky chtel dostat nejakou substituci, co se aplikuje na muj translate'env
-- resp jeho cast Kind'Env
-- abych si uz od ted nemyslel, ze (Maybe :: ?*) ale abych vedel ze to je * -> *
-- kdyz uz jsem v tom, bylo by taky fajn, kdyby v tom Translate'Env byly kinds trid?
-- teoreticky bych mohl v tomhle ohledu treatovat classy podobne jako typy
-- vzdycky by smely mit jenom jeden parametr - to by ale ostatne bylo vynuceny poctem promennych nad kteryma abstrahujou
-- ok to by asi slo

-- co s tou substituci?
-- 
-- no tak mozna bych budto mohl poskladat dohromady substituce a nebo posbirat constrainty a pak znova vyresit a ziskat kompletni substituci

-- infer'kinds :: Program -> Infer (Program, Subst Name Kind )
-- infer'kinds Program{ data'n'class'sections = d'n'c'sections } = do

  -- co se ted bude dit tady?

  -- zavolam nejakou tu funkci jako `infer'seq` a predam ji neco jako `infer'type'section'
  -- nepotrebuju aby to generovalo assumptions ohledne kindu
  -- protoze ja nemam zadny externi Kind Context
  -- vsechny kindy jsou ulozeny uvnitr Typu
  -- takze my asi staci, kdyz to vygeneruje [Constraint Kind]
  -- kdyz tyhle vyresim znova kompletne vsechny (je to agregace constraintu ze vsech sekci)
  -- dostanu super-substituci
  -- tu pak muzu aplikovat na [Data] a [Class]
  -- na to budu muset napsat instance
  -- 

  -- myslim ze bych jednak tu substituci mel aplikovat na data'n'class'sections a pak bych ji mel vratit
  -- abych ji pak venku mohl aplikovat na muj Trans'Env.kind'env
  -- diky tomu pak budu dalsi moduly parsovat a uz jim rovnou budu davat spravny kindy od zacatku

  -- undefined


infer'kinds :: Program -> Infer (Kind'Env, Constraint'Env, Subst Name Kind)
infer'kinds Program{ data'n'class'sections = type'sections } = do
  -- neni treba abych analyzoval method'annotations
  -- metody a jejich anotace jsou uz v classach uvnitr type'sections
  -- z anotaci metod samotnych urcim kind
  -- jelikoz neimplementuju kind polymorphism tak se muzu spolehnout na to, ze kazda typova promenna
  -- bude v ramci typove anotace zjevne indikovat svuj presny kind a pokud ne - defaulting ji zprisni na *
  --

  (kind'assumps, class'assumps, k'cs) <- infer'seq infer'type'section type'sections

  {-  NOTE: This is the TOP-LEVEL-SOLVE which takes all the constraints from all sections and solves them again.
            The goal is to obtain top-level substitution - that substitution will then be applied to parts of the Program. -}
  case run'solve k'cs :: Either Error (Subst Name Kind) of
    Left err -> throwError err
    Right subst -> do
      return  ( Map.fromList kind'assumps
              , Map.fromList class'assumps
              , subst )
