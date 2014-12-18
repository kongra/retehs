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
type Id = Int

newtype SymbolId     = SymbolId     Id     deriving Eq
newtype VariableId   = VariableId   Id     deriving Eq
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

newtype EnvIdState         = EnvIdState         Id
newtype EnvSymbolsRegistry = EnvSymbolsRegistry (Map.HashMap SymbolName Symbol)
newtype EnvWmesRegistry    = EnvWmesRegistry    (Map.HashMap WmeKey Wme)
newtype EnvWmesByObj       = EnvWmesByObj       (WmesIndex WmeObj)
newtype EnvWmesByAttr      = EnvWmesByAttr      (WmesIndex WmeAttr)
newtype EnvWmesByVal       = EnvWmesByVal       (WmesIndex WmeVal)
newtype EnvAmems           = EnvAmems           (Map.HashMap WmeKey Amem)
newtype EnvDtn             = EnvDtn             Dtn
newtype EnvDtt             = EnvDtt             Dtt
newtype EnvProductions     = EnvProductions     (Set.HashSet PNode)

-- | Environment
data Env =
  Env
  {
    -- | State of the Env-wide Id generator
    envIdState :: !(TVar EnvIdState)

    -- | Registry of (interned) Symbols
  , envSymbolsRegistry :: !(TVar EnvSymbolsRegistry)

    -- | All Wmes indexed by their WmeKey
  , envWmesRegistry :: !(TVar EnvWmesRegistry)

    -- 3 Wme indexes by Wme Field value
  , envWmesByObj  :: !(TVar EnvWmesByObj)
  , envWmesByAttr :: !(TVar EnvWmesByAttr)
  , envWmesByVal  :: !(TVar EnvWmesByVal)

    -- | Known alpha memories indexed by their WmeKey
  , envAmems :: !(TVar EnvAmems)

    -- Dummies
  , envDtn :: !Dtn
  , envDtt :: !Dtt

    -- | Productions the Env knows about
  , envProductions :: !(TVar EnvProductions)
  }

newtype WmeId             = WmeId             Id     deriving Eq
newtype WmeObj            = WmeObj            Symbol deriving Eq
newtype WmeAttr           = WmeAttr           Symbol deriving Eq
newtype WmeVal            = WmeVal            Symbol deriving Eq
newtype WmeAmems          = WmeAmems          [Amem]
newtype WmeToks           = WmeToks           (Set.HashSet Tok)
newtype WmeNegJoinResults = WmeNegJoinResults (Set.HashSet NegJoinResult)

-- | Working Memory Element
data Wme =
  Wme
  {
    wmeId :: !WmeId

  , wmeObj  :: !WmeObj
  , wmeAttr :: !WmeAttr
  , wmeVal  :: !WmeVal

    -- | α-memories this Wme belongs to (8 at most)
  , wmeAmems :: !(TVar WmeAmems)

    -- | Toks with tokenWme = this Wme
  , wmeToks :: !(TVar WmeToks)

    -- | Neg join results in which this Wme participates
  , wmeNegJoinResults :: !(TVar WmeNegJoinResults)
  }

instance Show Wme where
  show Wme { wmeObj = obj, wmeAttr = attr, wmeVal = val } =
    "(" ++ show obj  ++ "," ++ show attr ++ "," ++ show val  ++ ")"

instance Eq Wme where
  wme1 == wme2 = wmeId wme1 == wmeId wme2

instance Hashable Wme where
  hashWithSalt salt wme = salt `hashWithSalt` wmeId wme

instance Hashable WmeId where
  hashWithSalt salt (WmeId id') = salt `hashWithSalt` id'

instance Show WmeObj  where show (WmeObj  s) = show s
instance Show WmeAttr where show (WmeAttr s) = show s
instance Show WmeVal  where show (WmeVal  s) = show s

instance Hashable WmeObj where
  hashWithSalt salt (WmeObj s) = salt `hashWithSalt` s

instance Hashable WmeAttr where
  hashWithSalt salt (WmeAttr s) = salt `hashWithSalt` s

instance Hashable WmeVal where
  hashWithSalt salt (WmeVal s) = salt `hashWithSalt` s

newtype TokId             = TokId             Id          deriving Eq
newtype TokParent         = TokParent         GTok
newtype TokWme            = TokWme            (Maybe Wme)
newtype TokNode           = TokNode           Node
newtype TokChildren       = TokChildren       (Set.HashSet Tok)
newtype TokNegJoinResults = TokNegJoinResults (Set.HashSet NegJoinResult)
newtype TokNccResults     = TokNccResults     (Set.HashSet Tok)
newtype TokOwner          = TokOwner          (Maybe Tok)

-- | Token
data Tok =
  Tok
  {
    -- | An internal identifier of the token
    tokId :: !TokId

    -- | Points to a higher token
  , tokParent :: !TokParent

    -- | i-th Wme, Nothing for some toks
  , tokWme :: !TokWme

    -- | The node the token is in
  , tokNode :: !TokNode

    -- | The Toks with parent = this
  , tokChildren :: !(TVar TokChildren)

    -- | Used only for Toks in negative nodes
  , tokNegJoinResults :: !(TVar TokNegJoinResults)

    -- | Similar to tokNegJoinResults but for NCC nodes
  , tokNccResults :: !(TVar TokNccResults)

    -- | On Toks in NCC partners: toks in whose local memory this
    -- result resides
  , tokOwner :: !(TVar TokOwner)
  }

instance Eq Tok where
  Tok { tokId = id1 } == Tok { tokId = id2 } = id1 == id2

instance Hashable TokId where
  hashWithSalt salt (TokId id') = salt `hashWithSalt` id'

instance Hashable Tok where
  hashWithSalt salt Tok { tokId = id' } = salt `hashWithSalt` id'

newtype DttNode     = DttNode     Dtn
newtype DttChildren = DttChildren (Set.HashSet Tok)

-- | Dummy Top Token
data Dtt =
  Dtt
  {
    -- | The node the token is in - DummyTopNode
    dttNode :: !DttNode

    -- | The Toks with parent = this
  , dttChildren :: !DttChildren
  }

-- | Generalized Token
data GTok = Either Dtt Tok

-- | Field
data Field = Obj | Attr | Val deriving (Show, Eq)

-- | Index of Wmes by a specific field values
type WmesIndex a = Map.HashMap a (Set.HashSet Wme)

-- | A key for a Wme consisting of its obj, attr and value
data WmeKey = WmeKey !WmeObj !WmeAttr !WmeVal deriving Eq

instance Hashable WmeKey where
  hashWithSalt salt (WmeKey obj attr val) =
    salt `hashWithSalt` obj `hashWithSalt` attr `hashWithSalt` val

data NegJoinResult =
  NegJoinResult
  {
  }

data Amem =
  Amem
  {
  }

data Node =
  Node
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


-- Conds - TODO
