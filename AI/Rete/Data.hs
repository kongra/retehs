{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Rete.Data
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-12-15
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
------------------------------------------------------------------------
module AI.Rete.Data where

import           Control.Concurrent.STM (STM, TVar)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import           Data.Hashable (Hashable, hashWithSalt)
import qualified Data.Sequence as Seq

-- | Identifier type. We treat negative identifiers as special ones,
-- and the non-negative as auto-generated.
type ID = Int

newtype SymbolId     = SymbolId     ID     deriving Eq
newtype VariableId   = VariableId   ID     deriving Eq
newtype SymbolName   = SymbolName   String deriving Show
newtype VariableName = VariableName String deriving Show

instance Hashable SymbolId where
  hashWithSalt salt (SymbolId id') = salt `hashWithSalt` id'

instance Hashable VariableId where
  hashWithSalt salt (VariableId id') = salt `hashWithSalt` id'

-- | Symbols are the atomic pieces of information managed by Rete.
data Symbol = Symbol   !SymbolId   !SymbolName
            | Variable !VariableId !VariableName

instance Show Symbol where
  show (Symbol   _ s) = show s
  show (Variable _ s) = show s

instance Eq Symbol where
  (Symbol   id1 _) == (Symbol   id2 _) = id1 == id2
  (Variable id1 _) == (Variable id2 _) = id1 == id2
  _ == _ = False

instance Hashable Symbol where
  hashWithSalt salt (Symbol   id' _) = salt `hashWithSalt` id'
  hashWithSalt salt (Variable id' _) = salt `hashWithSalt` id'

newtype EnvIdState         = EnvIdState         ID
newtype EnvSymbolsRegistry = EnvSymbolsRegistry (Map.HashMap SymbolName Symbol)
newtype EnvWmesRegistry    = EnvWmesRegistry    (Map.HashMap WmeKey Wme)
newtype EnvWmesByObj       = EnvWmesByObj       WmesIndex
newtype EnvWmesByAttr      = EnvWmesByAttr      WmesIndex
newtype EnvWmesByVal       = EnvWmesByVal       WmesIndex
newtype EnvAmems           = EnvAmems           (Map.HashMap WmeKey Amem)
newtype EnvDtn             = EnvDtn             Dtn
newtype EnvDtt             = EnvDtt             Dtt
newtype EnvProductions     = EnvProductions     (Set.HashSet PNode)

-- | Environment
data Env =
  Env
  {
    -- State of the Env-wide ID generator
    envIdState :: !(TVar EnvIdState)

    -- Registry of (interned) Symbols
  , envSymbolsRegistry :: !(TVar EnvSymbolsRegistry)

    -- All Wmes indexed by their WmeKey
  , envWmesRegistry :: !(TVar EnvWmesRegistry)

    -- 3 Wme indexes by Wme Field value
  , envWmesByObj  :: !(TVar EnvWmesByObj)
  , envWmesByAttr :: !(TVar EnvWmesByAttr)
  , envWmesByVal  :: !(TVar EnvWmesByVal)

    -- Known alpha memories indexed by their WmeKey
  , envAmems :: !(TVar EnvAmems)

    -- Dummies
  , envDtn :: !Dtn
  , envDtt :: !Dtt

    -- Productions the Env knows about
  , envProductions :: !(TVar EnvProductions)
  }

-- | Working Memory Element
data Wme =
  Wme
  {
  }

data Tok =
  Tok
  {
  }

data Dtt =
  Dtt
  {
  }

data GTok = Either Dtt Tok

data Amem =
  Amem
  {
  }

data Dtn =
  Dtn
  {
  }

data PNode =
  PNode
  {
  }

-- newtype WmeId = WmeId
-- newtype WmeObj = WmeObj
-- newtype WmeAttr = WmeAttr
-- newtype WmeVal = WmeVal
-- newtype WmeAmems = WmeAmems
-- newtype WmeToks = WmeToks
-- newtype WmeNegJoinResults = WmeNegJoinResults

-- Tok
-- newtype TokId = TokId
-- newtype TokParent = TokParent
-- newtype TokWme = TokWme
-- newtype TokNode = TokNode
-- newtype TokChildren = TokChildren
-- newtype TokNegJoinResults = TokNegJoinResults
-- newtype TokNccResults = TokNccResults
-- newtype TokOwner = TokOwner

-- Dtt (Dummy Top Token)
-- DttNode -> Dtn
-- DttChildren

data Field = Obj | Attr | Val deriving (Show, Eq)

-- WmesIndex = Map.HashMap Symbol (Set.HashSet Wme)

-- Amem
-- newtype AmemSuccessors = AmemSuccessors
-- newtype AmemRefCount = AmemRefCount
-- newtype AmemWmes = AmemWmes
-- newtype AmemWmesByObj = AmemWmesByObj
-- newtype AmemWmesByAttr = AmemWmesByAttr
-- newtype AmemWmesByVal = AmemWmesByVal
-- newtype AmemObj = AmemObj
-- newtype AmemAttr = AmemAttr
-- newtype AmemVal = AmemVal

-- Dtn
-- newtype DtnChildren = DtnChildren
-- newtype DtnToks = DtnToks
-- newtype DtnAllChildren = DtnAllChildren

-- newtype BmemId = BmemId
-- newtype BmemParent = BmemParent
-- newtype BmemChildren = BmemChildren
-- newtype BmemToks = BmemToks
-- newtype BmemAllChildren = BmemAllChildren

-- newtype JoinNodeId = JoinNodeId
-- newtype JoinNodeParent = JoinNodeParent
-- newtype JoinNodeChildren = JoinNodeChildren
-- newtype JoinNodeAmem = JoinNodeAmem
-- newtype JoinNodeNearestAncestor = JoinNodeNearestAncestor
-- newtype JoinNodeJoinTests = JoinNodeJoinTests
-- newtype JoinNodeLeftUnlinked = JoinNodeLeftUnlinked
-- newtype JoinNodeRightUnlinked = JoinNodeRightUnlinked

-- newtype NegNodeId = NegNodeId
-- newtype NegNodeParent = NegNodeParent
-- newtype NegNodeChildren = NegNodeChildren
-- newtype NegNodeToks = NegNodeToks
-- newtype NegNodeAmem = NegNodeAmem
-- newtype NegNodeNearestAncestor = NegNodeNearestAncestor
-- newtype NegNodeJoinTests = NegNodeJoinTests
-- newtype NegNodeRightUnlinked = NegNodeRightUnlinked

-- newtype NccNodeId = NccNodeId
-- newtype NccNodeParent = NccNodeParent
-- newtype NccNodeChildren = NccNodeChildren
-- newtype NccNodeToks = NccNodeToks
-- newtype NccNodePartner = NccNodePartner

-- newtype NccPartnerId = NccPartnerId
-- newtype NccPartnerParent = NccPartnerParent
-- newtype NccPartnerChildren = NccPartnerChildren
-- newtype NccPartnerNccNode = NccPartnerNccNode
-- newtype NccPartnerNoc = NccPartnerNoc
-- newtype NccPartnerNewResultBuffer = NccPartnerNewResultBuffer

-- newtype PNodeId = PNodeId
-- newtype PNodeParent = PNodeParent
-- newtype PNodeChildren = PNodeChildren
-- newtype PNodeToks = PNodeToks
-- newtype PNodeAction = PNodeAction
-- newtype PNodeRevokeAction = PNodeRevokeAction
-- newtype PNodeBindings = PNodeBindings

-- -- Distance = Int

-- newtype JoinTestField1 = JoinTestField1
-- newtype JoinTestField2 = JoinTestField2
-- newtype JoinTestDistance = JoinTestDistance

-- newtype NegJoinResultOwner = NegJoinResultOwner
-- newtype NegJoinResultWme = NegJoinResultWme

-- newtype LocationField = LocationField
-- newtype LocationDistance = LocationDistance

-- Bindings

-- Actx
-- newtype ActxEnv = ActxEnv
-- newtype ActxNode = ActxNode
-- newtype ActxTok = ActxTok
-- newtype ActxWmes = ActxWmes

-- Action = Actx -> STM ()

data WmeKey = WmeKey !Symbol !Symbol !Symbol deriving Eq

type WmesIndex = Map.HashMap Symbol (Set.HashSet Wme)

-- Conds - TODO
