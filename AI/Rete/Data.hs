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

-- | Identifier type. We treat negative identifiers as special ones,
-- and the non-negative as auto-generated.
type ID = Int

newtype SymbolId     = SymbolId     ID
newtype VariableId   = VariableId   ID
newtype SymbolName   = SymbolName   String
newtype VariableName = VariableName String

-- | Symbols are the atomic pieces of information managed by Rete.
data Symbol = Symbol   !SymbolId   !SymbolName
            | Variable !VariableId !VariableName

-- Env
-- EnvIDState
-- EnvSymbolsRegistry
-- EnvWmesRegistry
-- EnvWmesByObj
-- EnvWmesByAttr
-- EnvWmesByVal
-- EnvAmems
-- EnvDtn
-- EnvDtt
-- EnvProductions

-- Wme
-- WmeId
-- WmeObj
-- WmeAttr
-- WmeVal
-- WmeAmems
-- WmeToks
-- WmeNegJoinResults

-- Tok
-- TokId
-- TokParent
-- TokWme
-- TokNode
-- TokChildren
-- TokNegJoinResults
-- TokNccResults
-- TokOwner

-- Dtt (Dummy Top Token)
-- DttNode -> Dtn
-- DttChildren

-- Field = Obj | Attr | Val deriving (Show, Eq)

-- WmesIndex = Map.HashMap Symbol (Set.HashSet Wme)

-- Amem
-- AmemSuccessors
-- AmemRefCount
-- AmemWmes
-- AmemWmesByObj
-- AmemWmesByAttr
-- AmemWmesByVal
-- AmemObj
-- AmemAttr
-- AmemVal

-- Dtn
-- DtnChildren
-- DtnToks
-- DtnAllChildren

-- BmemId
-- BmemParent
-- BmemChildren
-- BmemToks
-- BmemAllChildren

-- JoinNodeId
-- JoinNodeParent
-- JoinNodeChildren
-- JoinNodeAmem
-- JoinNodeNearestAncestor
-- JoinNodeJoinTests
-- JoinNodeLeftUnlinked
-- JoinNodeRightUnlinked

-- NegNodeId
-- NegNodeParent
-- NegNodeChildren
-- NegNodeToks
-- NegNodeAmem
-- NegNodeNearestAncestor
-- NegNodeJoinTests
-- NegNodeRightUnlinked

-- NccNodeId
-- NccNodeParent
-- NccNodeChildren
-- NccNodeToks
-- NccNodePartner

-- NccPartnerId
-- NccPartnerParent
-- NccPartnerChildren
-- NccPartnerNccNode
-- NccPartnerNoc (Number of Conjucts)
-- NccPartnerNewResultBuffer

-- PNodeId
-- PNodeParent
-- PNodeChildren
-- PNodeToks
-- PNodeAction
-- PNodeRevokeAction
-- PNodeBindings

-- Distance = Int

-- JoinTestField1
-- JoinTestField2
-- JoinTestDistance

-- NegJoinResultOwner
-- NegJoinResultWme

-- LocationField
-- LocationDistance

-- Bindings

-- Actx
-- ActxEnv
-- ActxNode
-- ActxTok
-- ActxWmes

-- Action = Actx -> STM ()

-- WmeKey = Symbol Symbol Symbol

-- Conds - TODO
